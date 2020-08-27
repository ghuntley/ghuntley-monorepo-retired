using Pulumi;
using Pulumi.Cloudflare;

partial class Infrastructure : Stack
{
    public void CloudflareDNS()
    {
        const string zoneId = "7823ac7e890b6b2fc0e8da4f16c999a4";

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

        #endregion


        #region domain ownership verification

        // new Record("dns-record-bing-verification", new RecordArgs
        // {
        //     Name = "",
        //     ZoneId = zoneId,
        //     Type = "CNAME",
        //     Value = "verify.bing.com",
        //     Ttl = 1, // 1 = automatic
        // });

        new Record("dns-record-google-site-verification", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "google-site-verification=Ygi199GbO9ipejg0519FZUH5JVbWB_qIS9JOke_ulp8",
            Ttl = 1, // 1 = automatic
        });

        // new Record("dns-record-keybase-verfication", new RecordArgs
        // {
        //     Name = "@",
        //     ZoneId = zoneId,
        //     Type = "TXT",
        //     Value = "",
        //     Ttl = 1, // 1 = automatic
        // });

        // new Record("dns-record-yandex-verification", new RecordArgs
        // {
        //     Name = "@",
        //     ZoneId = zoneId,
        //     Type = "TXT",
        //     Value = "",
        //     Ttl = 1, // 1 = automatic
        // });

        #endregion

        #region email

        new Record("dns-record-dkim", new RecordArgs
        {
            Name = "google._domainkey",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCgoXN71FIF9uJQI7jcD6OwsUXtgwqqtn9aK+cFekpP995w6haBNEI7gQPPEN4fiyvYJ0j6GmqMzOAdQNqseXYS2fEg7D2doZxNu6cA/VNd06XvhIKDvrKsdTkan9IgHuE3Ocrh/zMYNML4o4KvuAdb7ryGBTah7EEf5//MlWREpQIDAQAB",
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
