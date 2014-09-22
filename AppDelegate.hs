{-# LANGUAGE DataKinds, DeriveDataTypeable, EmptyDataDecls                  #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, QuasiQuotes          #-}
{-# LANGUAGE RecursiveDo, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE TypeOperators                                                  #-}
module AppDelegate (objc_initialise) where
import Messaging
import Particle

import Control.Applicative
import Control.Monad          (unless)
import Data.Proxy             (Proxy (..))
import Data.Typeable          (Typeable)
import Foreign                (nullPtr)
import Foreign                (withForeignPtr)
import FRP.Sodium
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC
import System.IO.Unsafe

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]

defineClass "NSObject"    Nothing
idMarshaller ''NSObject

defineClass "NSTimer" (Just ''NSObject)
idMarshaller ''NSTimer

defineClass "NSString"    (Just ''NSObject)
idMarshaller ''NSString

defineClass "NSView"   (Just ''NSObject)
idMarshaller ''NSView

defineClass "NSWindow"   (Just ''NSObject)
idMarshaller ''NSWindow

defineClass "NSControl"   (Just ''NSView)
idMarshaller ''NSControl

defineClass "NSTextField" (Just ''NSControl)
idMarshaller ''NSTextField

defineClass "MyView" (Just ''NSView)
idMarshaller ''MyView

defineSelector newSelector { reciever = (''NSTimer, "timer")
                           , selector = "timeInterval"
                           , returnType = Just [t| Double |]
                           , definition = [cexp| [timer timeInterval] |]
                           }

data Position = Position { xCoord :: Double, yCoord :: Double
                         } deriving (Read, Show, Eq, Ord, Typeable)


newtype MyPosition = MyPosition (ForeignPtr MyPosition)
                     deriving (Show, Eq, Ord, Typeable)

objc_typecheck

newPosition :: Double -> Double -> Position
newPosition = Position

objc_record "My" "Position" ''Position  [Typed 'newPosition]
  [ [objcprop| @property (readonly) double x; |] --> 'xCoord
  , [objcprop| @property (readonly) double y; |] --> 'yCoord
  ]
  [objcifdecls|
    + (instancetype)positionWithX:(double)x y:(double)y;
    + (instancetype)positionWithNSPoint:(typename NSPoint)pt;
  |]
  [objcimdecls|
    + (instancetype)positionWithX:(double)x y:(double)y
    {
       return [[MyPosition alloc] initWithPositionHsPtr: newPosition(x,y)];
    }
    + (instancetype)positionWithNSPoint:(typename NSPoint)pt {
       return [[MyPosition alloc] initWithPositionHsPtr: newPosition(pt.x,pt.y)];
    }
  |]

positionToMyPosition :: Position -> IO MyPosition
positionToMyPosition pos
  = $(objc ['pos :> ''Position] $
      Class ''MyPosition <: [cexp| [[MyPosition alloc] initWithPositionHsPtr: pos] |])

myPositionToPosition :: MyPosition -> IO Position
myPositionToPosition myPos
  = $(objc ['myPos :> Class ''MyPosition] $
            ''Position <: [cexp| newPosition([myPos x], [myPos y]) |])

objc_marshaller 'positionToMyPosition 'myPositionToPosition

newParticle :: Double -> Double -> Double -> Double -> Particle
newParticle x y dx dy = Particle (x,y) (dx,dy)

newtype MyParticle = MyParticle (ForeignPtr MyParticle)
                     deriving (Typeable, Show, Eq, Ord)

objc_typecheck

objc_record "My" "Particle" ''Particle  [Typed 'newParticle]
  [ [objcprop| @property (readonly) double x; |]
    ==> ([t| Double |], [| fst . pos |], [| \p x -> p { pos = (x, snd $ pos p) } |])
  , [objcprop| @property (readonly) double y; |]
    ==> ([t| Double |], [| snd . pos |], [| \p y -> p { pos = (fst $ pos p, y) } |])
  , [objcprop| @property (readonly) double velX; |]
    ==> ([t| Double |], [| fst . velocity |], [| \p x -> p { velocity = (x, snd $ velocity p) } |])
  , [objcprop| @property (readonly) double velY; |]
    ==> ([t| Double |], [| snd . velocity |], [| \p y -> p { velocity = (fst $ velocity p, y) } |])
  ]
  [objcifdecls|
    + (instancetype)particleWithX:(double)x y:(double)y velX:(double)dx velY:(double)dy;
  |]
  [objcimdecls|
    + (instancetype)particleWithX:(double)x y:(double)y velX:(double)dx velY:(double)dy
    {
       return [[MyParticle alloc] initWithParticleHsPtr: newParticle(x,y,dx,dy)];
    }
  |]

particleToMyParticle :: Particle -> IO MyParticle
particleToMyParticle pos
  = $(objc ['pos :> ''Particle] $
      Class ''MyParticle <: [cexp| [[MyParticle alloc] initWithParticleHsPtr: pos] |])

myParticleToParticle :: MyParticle -> IO Particle
myParticleToParticle myPos
  = $(objc ['myPos :> Class ''MyParticle] $
            ''Particle <: [cexp| newParticle([myPos x], [myPos y], [myPos velX], [myPos velY]) |])

objc_marshaller 'particleToMyParticle 'myParticleToParticle

newSize :: Double -> Double -> Size
newSize = Size

newtype MySize = MySize (ForeignPtr MySize)
               deriving (Typeable, Eq, Ord, Show)

objc_typecheck

objc_record "My" "Size" ''Size  [Typed 'newSize]
  [ [objcprop| @property (readonly) double width; |] --> 'width
  , [objcprop| @property (readonly) double height; |] --> 'height
  ]
  [objcifdecls|
    + (instancetype)sizeWithWidth:(float)w height:(float)h;
    + (instancetype)sizeWithNSSize:(typename NSSize)size;
  |]
  [objcimdecls|
    + (instancetype)sizeWithWidth:(float)w height:(float)h
    {
       return [[MySize alloc] initWithSizeHsPtr: newSize(w,h)];
    }
    + (instancetype)sizeWithNSSize:(typename NSSize)size {
       return [[MySize alloc] initWithSizeHsPtr: newSize(size.width,size.height)];
    }
  |]

sizeToMySize :: Size -> IO MySize
sizeToMySize size
  = $(objc ['size :> ''Size] $
      Class ''MySize <: [cexp| [[MySize alloc] initWithSizeHsPtr: size] |])

mySizeToSize :: MySize -> IO Size
mySizeToSize mySize
  = $(objc ['mySize :> Class ''MySize] $
            ''Size <: [cexp| newSize([mySize width], [mySize height]) |])

objc_marshaller 'sizeToMySize 'mySizeToSize

data Rect = Rect { origin :: Position, size :: Size
                 } deriving (Read, Show, Eq, Ord, Typeable)


newRect :: Position -> Size -> Rect
newRect = Rect

newtype MyRect = MyRect (ForeignPtr MyRect)
               deriving (Typeable, Eq, Ord, Show)

objc_typecheck

objc_record "My" "Rect" ''Rect  [Typed 'newRect]
  [ [objcprop| @property (readonly) typename MyPosition *origin; |]
    --> 'origin
  , [objcprop| @property (readonly) typename MySize *size; |]
    --> 'size
  ]
  [objcifdecls|
    + (instancetype)rectWithNSRect:(typename NSRect)rect;
  |]
  [objcimdecls|
    + (instancetype)rectWithNSRect:(typename NSRect)rect {
      return [[MyRect alloc] initWithRectHsPtr: newRect([MyPosition positionWithNSPoint: rect.origin],
                                                        [MySize sizeWithNSSize: rect.size])];
    }
  |]

rectToMyRect :: Rect -> IO MyRect
rectToMyRect rect
  = $(objc ['rect :> ''Rect] $
      Class ''MyRect <: [cexp| [[MyRect alloc] initWithRectHsPtr: rect] |])

myRectToRect :: MyRect -> IO Rect
myRectToRect myRect
  = $(objc ['myRect :> Class ''MyRect] $
            ''Rect <: [cexp| newRect([myRect origin], [myRect size]) |])

objc_marshaller 'rectToMyRect 'myRectToRect

defineSelector newSelector { selector = "doubleValue"
                           , reciever = (''NSControl, "ctrl")
                           , returnType = Just [t| Double |]
                           , definition = [cexp| [ctrl doubleValue] |]
                           }

defineClass "NSEvent" (Just ''NSObject)
idMarshaller ''NSEvent

defineSelector newSelector { selector = "bounds"
                           , reciever = (''NSView, "view")
                           , returnType = Just [t| Rect |]
                           , definition = [cexp| [MyRect rectWithNSRect: [view bounds]] |]
                           }
nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

objc_interface [cunit|
@interface MyView : NSView

// IBOutlets
@property (retain,nonatomic) typename MyParticle *particle;

// IBActions
- (void)drawRect:(typename NSRect)dirtyRect;
@end
|]

objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate>

// IBOutlets
@property (assign,nonatomic) typename NSWindow *window;
@property (assign,nonatomic) typename MyView   *mainView;
@property (assign) typename NSTimer *curTimer;
@property (assign,nonatomic) double interval;

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification;
- (void)fire: (typename NSTimer *)timer;

@end
|]

defineSelector newSelector { selector = "setParticle"
                           , reciever = (''MyView, "view")
                           , arguments = ["part" :>>: ''Particle ]
                           , definition = [cexp| [view setParticle: part] |]
                           }

type Listener a = a -> IO ()

data Session = Session { pushInterval_ :: Listener Double
                       , application   :: AppDelegate
                       } deriving (Typeable)

pushInterval :: Session -> Double -> IO ()
pushInterval = pushInterval_

newtype AppDelegate = AppDelegate (ForeignPtr AppDelegate)
                      deriving (Typeable, Show, Eq, Ord)

marshalAppDelegate :: AppDelegate -> IO AppDelegate
marshalAppDelegate = return

idMarshaller ''AppDelegate

mainWindow :: AppDelegate -> IO NSWindow
mainWindow app = $(objc ['app :> ''AppDelegate] $
                   Class ''NSWindow <: [cexp| [app window] |])

mainView :: AppDelegate -> IO MyView
mainView app = $(objc ['app :> ''AppDelegate] $
                     Class ''MyView <: [cexp| [app mainView] |])

curTimer :: AppDelegate -> IO NSTimer
curTimer app = $(objc ['app :> ''AppDelegate] $
                     Class ''NSTimer <: [cexp| [app curTimer] |])

defineSelector newSelector { selector = "display"
                           , reciever = (''NSView, "view")
                           , definition = [cexp| [view display] |]
                           }


setTimer :: AppDelegate -> NSTimer -> IO ()
setTimer app timer = $(objc ['app :> ''AppDelegate, 'timer :> Class ''NSTimer] $
                       void [cexp| [app setCurTimer: timer ] |])

calling :: (NSTimer -> IO ()) -> NSTimer -> IO ()
calling = ($)

objc_typecheck

newSession :: AppDelegate -> IO Session
newSession app = do
  view <- mainView app
  area <- size <$> (view # bounds)
  part0 <- randomParticle area
  sess <- sync $ do
    (timeB, push) <- newBehaviour 0
    partB <- collect (\dur p0 -> let p' = updParticle area p0 dur
                                 in (p', p')) part0 timeB
    listen (value partB) $ \part -> do
      view # setParticle part
      view # display
    return $ Session (sync . push) app
  return sess

objc_implementation [Typed 'pushInterval, Typed 'newSession]
  [cunit|
@interface AppDelegate ()
// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr session;
@end

@implementation AppDelegate
@synthesize interval = _interval;

- (void)setInterval:(double)ints
{
  _interval = ints;
  [self startTimer];
}

- (double)interval
{
  return _interval;
}

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.session = newSession(self);
  [self        bind: @"interval"
           toObject: [NSUserDefaultsController sharedUserDefaultsController]
        withKeyPath: @"values.fps"
            options: nil];
}

- (void)startTimer
{
  if (self.curTimer) { [self.curTimer invalidate];}
  self.curTimer = [NSTimer scheduledTimerWithTimeInterval: 1/self.interval
                   target: self
                   selector: @selector(fire:) userInfo: nil repeats: YES];
}
- (void)fire: (typename NSTimer *)timer
{
  pushInterval(self.session, [timer timeInterval]);
}

@end
|]

defineClass "NSColor" (Just ''NSObject)
idMarshaller ''NSColor

defineSelector newSelector { selector = "set"
                           , reciever = (''NSColor, "color")
                           , definition = [cexp| [color set] |]
                           }

defineSelector newSelector { selector = "getParticle"
                           , reciever = (''MyView, "view")
                           , returnType = Just [t| Particle |]
                           , definition = [cexp| view.particle |]
                           }

whiteColor :: NSColor
whiteColor = unsafePerformIO $(objc [] $ Class ''NSColor <: [cexp| [NSColor whiteColor] |])
{-# NOINLINE whiteColor #-}

blueColor :: NSColor
blueColor = unsafePerformIO $(objc [] $ Class ''NSColor <: [cexp| [NSColor blueColor] |])
{-# NOINLINE blueColor #-}

rectFill :: Rect -> IO ()
rectFill rect
  = $(objc ['rect :> ''Rect] $ void
      [cexp| NSRectFill(NSMakeRect(rect.origin.x, rect.origin.y,
                                   rect.size.width, rect.size.height)) |])

defineClass "NSBezierPath" (Just ''NSObject)
idMarshaller ''NSBezierPath

defineSelector newSelector { selector = "fill"
                           , reciever = (''NSBezierPath, "thePath")
                           , definition = [cexp| [thePath fill] |]
                           }

ovalInRect :: Rect -> IO NSBezierPath
ovalInRect rect
  = $(objc ['rect :> ''Rect] $ Class ''NSBezierPath <: [cexp|
      [NSBezierPath bezierPathWithOvalInRect: NSMakeRect(rect.origin.x, rect.origin.y,
                                                         rect.size.width, rect.size.height)] |])

drawParticle :: MyView -> Rect -> IO ()
drawParticle self rect = do
  whiteColor # set
  rectFill =<< self # bounds
  blueColor # set
  Particle (x, y) _ <- self # getParticle
  ovalInRect Rect { origin = Position (x - 5) (y - 5)
                  , size   = Size 10 10
                  }
    #. fill

objc_implementation [Typed 'drawParticle]
  [cunit|
@implementation MyView

- (void)drawRect:(typename NSRect)dirtyRect
  {
    drawParticle(self, [MyRect rectWithNSRect: dirtyRect]);
  }
@end
   |]

objc_emit

