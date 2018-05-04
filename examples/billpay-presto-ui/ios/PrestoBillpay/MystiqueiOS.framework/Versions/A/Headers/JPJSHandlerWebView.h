//
//  JSHandlerWebView.h
//  JuspaySafeBrowser
//
//  Created by Sachin Sharma on 19/1/16.
//  Copyright © 2016 Juspay Technologies Pvt Ltd. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <WebKit/WebKit.h>
#import <objc/runtime.h>

@class JPJSHandlerWebViewProxyDelegate;

@interface JPJSHandlerWebView : WKWebView<UIGestureRecognizerDelegate,WKNavigationDelegate>

// All the events will pass through this proxy delegate first
@property (nonatomic, retain) JPJSHandlerWebViewProxyDelegate* proxyDelegate;
@property (nonatomic, strong) NSString* additinalmmm;

- (void) addJavascriptInterfaces:(NSObject*) interface WithName:(NSString*) name;

- (NSString *)stringByEvaluatingJavaScriptFromString:(NSString *)script errorLog:(NSString*)logsString;

@end
