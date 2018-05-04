//
//  ViewController.m
//  PrestoBillpay
//
//  Created by Balaganesh S on 27/04/18.
//  Copyright © 2018 Juspay. All rights reserved.
//

#import "ViewController.h"
#import "JSInterface.h"
#import <MystiqueiOS/MystiqueiOS.h>
#import <MystiqueiOS/MystiqueSingleton.h>

@interface ViewController ()<MystiqueiOSDelegate>

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    
    MystiqueiOS *mystique = [[MystiqueiOS alloc] init];
    mystique.delegate = self;
    mystique.webView.additinalmmm = @"";
    
    mystique.containerView = self.view;
    mystique.containerViewController = self;
    mystique.webView.UIDelegate=SharedMystique;
    SharedMystique.mystique = mystique;
    SharedMystique.allowedImageExtensions = @[@"pdf",@"png"];
    
    JSInterface *jsInterface = [[JSInterface alloc] init];
    
    [SharedMystique.mystique.webView addJavascriptInterfaces:jsInterface WithName:@"JBridge"];
    [SharedMystique.jsInterfaces addObject:jsInterface];
    
    NSString *fileContent = [NSString stringWithContentsOfFile:[[NSBundle mainBundle] pathForResource:@"index_bundle" ofType:@"js"] encoding:NSUTF8StringEncoding  error:nil];
    [SharedMystique.mystique loadJS:fileContent];

}

- (void)handleSingleTap:(UITapGestureRecognizer *)recognizer
{
    [self.view endEditing:YES];
}

- (void)didReceiveMemoryWarning {
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}


@end
