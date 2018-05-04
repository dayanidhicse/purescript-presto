//
//  AppDelegate.h
//  PrestoBillpay
//
//  Created by Balaganesh S on 27/04/18.
//  Copyright © 2018 Juspay. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <CoreData/CoreData.h>

@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property (strong, nonatomic) UIWindow *window;

@property (readonly, strong) NSPersistentContainer *persistentContainer;

- (void)saveContext;


@end

