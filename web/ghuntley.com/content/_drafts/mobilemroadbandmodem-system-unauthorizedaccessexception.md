# MobileBroadBandModem System.UnauthorizedAccessException

Turns out access to simcard information is retricted in Windows 10 applications, sigh.

https://msdn.microsoft.com/en-us/library/windows/hardware/dn391779.aspx

https://msdn.microsoft.com/en-us/library/windows/apps/mt270968.aspx

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
