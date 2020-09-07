# Design Notes

- Reminder: It is possible for a handset to contain multiple SIM cards.
- Reminder: It is possible for a single sim to have multiple service (phone) numbers.

# Specifications
- http://www.ebay.com/gds/SIM-Card-Guide-/10000000177629426/g.html
- https://en.wikipedia.org/wiki/Subscriber_identity_module
- https://github.com/SecUpwN/Android-IMSI-Catcher-Detector

# Android
- Requires the `READ_PHONE_STATE` permission to be defined in the AppManifest.xml
- https://stackoverflow.com/questions/9751823/how-can-i-get-the-iccid-number-of-the-phone
- http://www.anddev.org/tinytut_-_getting_the_imsi_-_imei_sim-device_unique_ids-t446.html
- https://developer.android.com/reference/android/telephony/TelephonyManager.html#getNetworkOperatorName()
- https://stackoverflow.com/questions/14051023/how-to-get-current-sim-card-number-in-android
- https://forums.xamarin.com/discussion/28709/how-to-read-imei-in-dual-sim-phone
- https://github.com/pbakondy/cordova-plugin-sim/blob/master/src/android/com/pbakondy/Sim.java

# iOS
- https://stackoverflow.com/questions/18972242/use-private-api-for-inhouse-apps-to-get-imei-iccid-and-imsi
- http://pastebin.com/vkAUfLZw

- https://stackoverflow.com/questions/16667988/how-to-get-imei-on-iphone-5

> On iOS 7 these APIs are protected by com.apple.coretelephony.Identity.get entitlement. To access IMEI (IMSI, phone number and other info) you need to sign your with that entitlement with boolean value set to true.

- http://stackoverflow.com/a/27312697
- https://stackoverflow.com/questions/27312352/accessing-sim-info-programatically-in-iphone
- https://github.com/Cykey/ios-reversed-headers/blob/master/CoreTelephony/CTSIMSupport.h

- https://stackoverflow.com/questions/21314439/how-to-get-the-iccid-of-the-sim-card-that-is-in-the-iphone-using-my-ios-app
- https://github.com/pbakondy/cordova-plugin-sim/blob/master/src/ios/Sim.m

# Universal Windows Platform
- https://msdn.microsoft.com/en-us/library/windows/apps/mt270968.aspx

> Special and restricted capabilities are intended for very specific scenarios. The use of these capabilities is highly restricted and subject to additional Store onboarding policy and review.
>  
> There are cases where such capabilities are necessary and appropriate, such as banking with two-factor authentication, where users provide a smart card with a digital certificate that confirms their identity. Other apps may be designed primarily for enterprise customers and may need access to corporate resources that cannot be accessed without the user’s domain credentials.
> Apps that apply the special-use capabilities require a company account to submit them to the Store. In contrast, restricted capabilities do not require a special company account for the Store, they are not available for developers to use. Restricted capabilities are available only to apps that are developed by Microsoft and its partners. For more information about company accounts, see Account types, locations, and fees.
> All restricted capabilities must include the rescap namespace when you declare them in your app's package manifest differently than other capabilities. The following example shows you how to declare the appCaptureSettings capability.

    Package.appxmanifest

	xmlns:rescap="http://schemas.microsoft.com/appx/manifest/foundation/windows10/restrictedcapabilities" 
	
    <Capabilities>
        <rescap:Capability Name="cellularDeviceIdentity" />
    </Capabilities>


- https://wpdev.uservoice.com/forums/110705-universal-windows-platform/suggestions/4037250-api-to-retrieve-imei-imsi-mcc-mnc-and-msisdn
- https://stackoverflow.com/questions/12534641/is-an-ashwid-guaranteed-to-uniquely-identify-a-device
- http://www.wadewegner.com/2012/09/getting-the-application-id-and-hardware-id-in-windows-store-applications/

# Windows Phone

> Because it has methods that can be used to get personally identifiable information about the customer and change the network settings on mobile broadband devices, the Mobile Broadband Account API is a privileged API. This means that most Windows Store apps cannot call its methods without getting an “access denied” error. To be able to call this API, a Windows Store app must meet the following criteria:
- https://msdn.microsoft.com/en-us/library/windows/hardware/dn391779.aspx

- https://github.com/pbakondy/cordova-plugin-sim/blob/master/src/wp/Sim.cs
- https://social.msdn.microsoft.com/Forums/windowsapps/en-US/4ff692bc-97c9-4943-b1ee-ec4f098e3b14/how-to-detect-sim-card-change-or-imsi-change-programatically-in-windows-phone-8?forum=wpdevelop