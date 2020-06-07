# Display listing of emulators 

  PS > get-vm
  
  Name                         State   CPUUsage(%) MemoryAssigned(M) Uptime           Status             Version
  ----                         -----   ----------- ----------------- ------           ------             -------
  Emulator WVGA 512MB.ghuntley Running 0           512               00:01:06.7860000 Operating normally 7.0

# Display listing of VSwitches

  PS> Get-VMSwitch
  
  Name                                                    SwitchType NetAdapterInterfaceDescription
  ----                                                    ---------- ------------------------------
  VirtualBox Host-Only Ethernet Adapter Virtual Switch    External   VirtualBox Host-Only Ethernet Adapter
  VirtualBox Host-Only Ethernet Adapter #2 Virtual Switch External   VirtualBox Host-Only Ethernet Adapter #2
  Broadcom 802.11ac Network Adapter #2 Virtual Switch     External   Broadcom 802.11ac Network Adapter #2
  Windows Phone Emulator Internal Switch                  Internal
  
  
# Reference
* https://technet.microsoft.com/en-us/library/hh848573.aspx
