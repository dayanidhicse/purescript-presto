//
//  MystiqueiOS.h
//  MystiqueiOS
//
//  Created by Sachin Sharma on 27/09/16.
//  Copyright © 2016 Juspay Technologies Pvt Ltd. All rights reserved.
//

#import <UIKit/UIKit.h>
#include <WebKit/WebKit.h>
#import "JPJSHandlerWebView.h"

#pragma mark - Mystique View Controller

@protocol ViewControllerDelegate <NSObject>
@optional
- (UIImage*)imageForName:(NSString*)name size:(CGRect)size;
@end

@interface CustomNavigationController : UINavigationController

@end


#pragma mark - Mystique Delegate
@protocol MystiqueiOSDelegate <NSObject>
@optional
- (void)userContentController:(WKUserContentController *)userContentController
      didReceiveScriptMessage:(WKScriptMessage *)message;
- (UIImage*)imageNamed:(NSString*)imageName;
- (UIImage*)imageNamed:(NSString *)imageName withSize:(CGRect)frame;

@end

@protocol MystiqueCoreDelegate <NSObject>

@optional
- (void)scripMessageCalled:(NSString*)scriptMessage;
@end

@interface MystiqueiOS : NSObject
@property (nonatomic, strong) UIView *containerView;
@property (nonatomic, strong) UIViewController *containerViewController;
@property (nonatomic, strong) NSMutableArray *viewControllers;
@property (nonatomic, strong) NSArray *customFonts;
@property (nonatomic, strong) NSString *serverURL;
@property (nonatomic, strong) NSBundle *resourceBundle;
@property (nonatomic, strong) JPJSHandlerWebView *webView;
@property (nonatomic, assign) id <MystiqueiOSDelegate>delegate;
@property (nonatomic, assign) id <MystiqueCoreDelegate>coreDelegate;
- (void)loadJS:(NSString *)JS;
- (void)render:(NSDictionary*)data;
- (void)modifyObject:(NSDictionary*)data;
- (void)addMessagHandler:(NSString*)handle;
- (void)addJsToWebView:(NSString *)JS;
- (void)addHtmlToWebView:(NSString *)html;
- (void)renderObjectForCommands:(NSDictionary*)ui  withObject:(id)object andType:(NSString*) type;
- (void)popToViewControllerAtIndex:(int)index;
@end
