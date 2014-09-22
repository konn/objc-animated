//
//  AppDelegate.h
//  Animated
//
//  Created by 石井 大海 on 2014/09/14.
//  Copyright (c) 2014年 Hiromi ISHII. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "MyView.h"

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet MyView *mainView;

@end
