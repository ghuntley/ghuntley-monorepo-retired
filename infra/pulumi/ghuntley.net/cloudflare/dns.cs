using Pulumi;
using Pulumi.Cloudflare;

partial class Infrastructure : Stack
{
    public void CloudflareDNS()
    {
        const string zoneId = "8cbecb7f14bf65e2e7060b325d928340";

        #region hosts

        new Record("dns-record-devenv", new RecordArgs
        {
            Name = "devenv",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.98.111.64",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-surfacego", new RecordArgs
        {
            Name = "surfacego",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.77.47.105",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-metabox", new RecordArgs
        {
            Name = "metabox",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.64.69.62",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-smtp", new RecordArgs
        {
            Name = "smtp",
            ZoneId = zoneId,
            Type = "A",
            Value = "127.0.0.1",
            Ttl = 1, // 1 = automatic
        });

        #endregion

        #region domain ownership verification

        new Record("dns-record-google-site-verification", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "google-site-verification=rx0xZRxwl5gGW49-MTeR8j34i4GKcqFdp0-TiwxJ3cs",
            Ttl = 1, // 1 = automatic
        });

        #endregion

        #region email

        new Record("dns-record-dkim", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "TODO2",
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
