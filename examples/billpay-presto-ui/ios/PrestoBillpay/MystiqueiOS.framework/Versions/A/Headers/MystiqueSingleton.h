//
//  MystiqueSingleton.h
//  MystiqueiOS
//
//  Created by Sachin Sharma on 02/01/17.
//  Copyright © 2017 Juspay Technologies Pvt Ltd. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "MystiqueiOS.h"

#define SharedMystique [MystiqueSingleton sharedInstance]

@interface MystiqueSingleton : UIView<WKUIDelegate>
@property (nonatomic, strong) MystiqueiOS *mystique;
@property (nonatomic, strong) id bundle;
@property (nonatomic, strong) UIAlertController *alertController;
@property (nonatomic, strong) NSString *alertControllerCallback;
@property (nonatomic, strong) NSArray *allowedImageExtensions;
@property (nonatomic) Boolean shouldNotTriggerLockScreen;
@property (nonatomic) Boolean passcodeScreenTriggered;
@property (nonatomic, strong) NSMutableArray *jsInterfaces;

+(MystiqueSingleton*)sharedInstance;

@end
