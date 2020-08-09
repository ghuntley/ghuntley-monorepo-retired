using Pulumi;
using Pulumi.Cloudflare;

partial class Infrastructure : Stack
{
    public void CloudflareDNS()
    {
        const string zoneId = "a548e719763b08ac695475735536d9ad";

        #region hosts

        new Record("dns-record-auth", new RecordArgs
        {
            Name = "auth",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "ghuntley.dev",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-root", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "devenv.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

//       new Record("dns-record-root", new RecordArgs
//       {
//          Name = "@",
//          ZoneId = zoneId,
//          Type = "CNAME",
//          Value = "p52157.probes.atlas.ripe.net",
//          Ttl = 1, // 1 = automatic
//       });

        new Record("dns-record-www", new RecordArgs
        {
            Name = "www",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "ghuntley.dev",
            Ttl = 1, // 1 = automatic
        });

        #endregion

        #region domain ownership verification

        new Record("dns-record-acme-verification", new RecordArgs
        {
            Name = "_acme-challenge",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "562ecdbe-d9eb-4ca8-bb6f-e7f49630aa0b.auth.acme-dns.io",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-google-site-verification", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "google-site-verification=ooojce4hslC8h2R84k1Kvt0GrtjycD3oC7YLv_vjjEI",
            Ttl = 1, // 1 = automatic
        });

        #endregion

        #region email

        new Record("dns-record-dkim", new RecordArgs
        {
            Name = "google._domainkey",
            ZoneId = zoneId,
            Type = "TXT",
            Value = "v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAza6YHbwelIxuUHkPmpt9v7IoDLgKyzsmZ4+IO5HnSPTAz2Tuxn5wsnvjP6bvmZwGZYSIW7RJA1UNr4sWlIg48S4DvSd90reDlAcaaDNEsbhzlKzl1WCJTvUkuhqO11DonlkMcEpdQI9xzH5vZyLP6qksKn3/Wyy0Qq4GzdYqst5Ml9GHSn3rTpveD7k7D1WLt9mSpi+/wjbMZ8i67tu88WmGT+6uR6BJ+dtZmC7UaiV+XuGfQIlpCgBj4+kwRFL5dNEA6R6gMMHRxFm2OPs9S8onkqExnIYQMKW9VyToOdEIAp+o+FunJTMoBfhmkN3+d4nIlo6dg/ms78qro/pTpQIDAQAB",
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
