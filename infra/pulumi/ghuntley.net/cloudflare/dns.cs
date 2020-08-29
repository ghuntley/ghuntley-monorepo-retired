using Pulumi;
using Pulumi.Cloudflare;

partial class Infrastructure : Stack
{
    public void CloudflareDNS()
    {
        const string zoneId = "8cbecb7f14bf65e2e7060b325d928340";

        #region hosts

        new Record("dns-record-devenv-public", new RecordArgs
        {
            Name = "public.devenv",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "devenv.australiaeast.cloudapp.azure.com",
            Ttl = 1, // 1 = automatic
        });


        new Record("dns-record-devenv", new RecordArgs
        {
            Name = "devenv",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.74.246.124",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-compute", new RecordArgs
        {
            Name = "devenv",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.88.32.97",
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

        new Record("dns-record-pixel4xl", new RecordArgs
        {
            Name = "pixel4xl",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.123.145.101",
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

        new Record("dns-record-nexus", new RecordArgs
        {
            Name = "nexus",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.ghuntley.net",
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
            Name = "google._domainkey",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAnd/I9StH7axKnbH85OkB4dxaGpkXsh2vWnmCLE0G2HRR7/cAnl8d9lzUBXF+aG4n//okWHoaHPmfLLGZNXs/srFIVAqX1deu1tPS7pqph6EHd9tzEV/KJ9Pg0bxNTZ9nFpsqP11UxfZMj6YlIoBvM0MT8RQ91Rk4zxMJuXA1EeJ10HNYHQANHt+W1jknNEAFODhN2h2f6SPeWgIK8viYGsVj+w+iVkl9gLtDE28qInpHkpaj1CoWE8W/bl9IYNOs5lrCmYJD84y36lDVL501Nttyh+Jpa4+ktr3XcqBwjmrH4hJxyyL0g4ufpMcN53GVqIK3YU75n03ZFWrq/Mq7wQIDAQAB",
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
