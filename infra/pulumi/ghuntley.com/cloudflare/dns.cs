using Pulumi;
using Pulumi.Cloudflare;

partial class Infrastructure : Stack
{
    public void CloudflareDNS()
    {
        const string zoneId = "9013e73e470b50749a8c7dbbc09da6b1";

        #region hosts

        new Record("dns-record-root", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "ghuntley.netlify.com",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-www", new RecordArgs
        {
            Name = "www",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "ghuntley.netlify.com",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-home", new RecordArgs
        {
            Name = "home",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "remote.nabucasa.com",
            Ttl = 1, // 1 = automatic
        });

        #endregion

        #region google apps

        new Record("dns-record-calendar", new RecordArgs
        {
            Name = "calendar",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "ghs.google.com",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-docs", new RecordArgs
        {
            Name = "docs",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "ghs.google.com",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-mail", new RecordArgs
        {
            Name = "mail",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "ghs.google.com",
            Ttl = 1, // 1 = automatic
        });

        #endregion

        #region domain ownership verification

        new Record("dns-record-bing-verification", new RecordArgs
        {
            Name = "aad95f930bf299e7925ec84a9fd86245",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "verify.bing.com",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-google-site-verification", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "google-site-verification=Ygi199GbO9ipejg0519FZUH5JVbWB_qIS9JOke_ulp8",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-keybase-verfication", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "keybase-site-verification=qINp7lIpBcjEroqO0EFwILvIXlf1pWb0gpAEB3OhQn8",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-yandex-verification", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "yandex-verification: 13b44c9f1d9d0c8b",
            Ttl = 1, // 1 = automatic
        });

        #endregion

        #region email

        new Record("dns-record-dkim", new RecordArgs
        {
            Name = "google._domainkey",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAjEiBF+hS+JfkK44XK+bCQZgs+l/+SuTXGzGxzlOLDXygZeQ21e9XUrHZ3KnqNTZTKmxbp2LhOGsetN2DRSybkDngv6Xue64BHHd3Y0lhHogr7l68fMI0JH5cDR2KETBunA4WB2FWtAqLVRWfdZxqMvYygF1sJjX5UKg1R/IOXtrx+5/qte8kfTRStt+UqBvra14n1WBnybwgV35+1447s4UGwzd2du5I1fOfX3zUzyho6+NpDerklwHvOyHIU545uwySxutdGb83AKSJSaGfPxVnfw1RYj7A/ckr4AMY60lDBsiEHpwzTLYOdH1elfdQKK+VtPwneCfIIebdsi2ROwIDAQAB",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-spf", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "v=spf1 include:_spf.google.com ~all",
            Ttl = 1, // 1 = automatic
        });


        new Record("dns-record-mx-1", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "MX",
            Value = "aspmx.l.google.com",
            Priority = 1,
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-mx-5-1", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "MX",
            Value = "alt1.aspmx.l.google.com",
            Priority = 5,
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-mx-5-2", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "MX",
            Value = "alt2.aspmx.l.google.com",
            Priority = 5,
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-mx-10-1", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "MX",
            Value = "alt3.aspmx.l.google.com",
            Priority = 10,
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-mx-10-2", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "MX",
            Value = "alt4.aspmx.l.google.com",
            Priority = 10,
            Ttl = 1, // 1 = automatic
        });

        #endregion
    }
}
