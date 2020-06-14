using Pulumi;
// using Pulumi.Cloudflare;

partial class DepotStack : Stack
{
    public DepotStack()
    {
        const string zoneId = "8cbecb7f14bf65e2e7060b325d928340";

        // new Cloudflare.Record("@", new Cloudflare.RecordArgs
        //     {
        //         Name = "@",
        //         ZoneId = zoneId,
        //         Type = "MX",
        //         Value = "aspmx2.googlemail.com",
        //         Priority = 10,
        //         Ttl = 1, // 1 = automatic
        //     });

        // new Cloudflare.Record("@", new Cloudflare.RecordArgs
        //     {
        //         Name = "@",
        //         ZoneId = zoneId,
        //         Type = "MX",
        //         Value = "alt2.aspmx.l.google.com",
        //         Priority = 5,
        //         Ttl = 1, // 1 = automatic
        //     });

        // new Cloudflare.Record("@", new Cloudflare.RecordArgs
        //     {
        //         Name = "@",
        //         ZoneId = zoneId,
        //         Type = "MX",
        //         Value = "alt1.aspmx.l.google.com",
        //         Priority = 5,
        //         Ttl = 1, // 1 = automatic
        //     });

        // new Cloudflare.Record("@", new Cloudflare.RecordArgs
        //     {
        //         Name = "@",
        //         ZoneId = zoneId,
        //         Type = "MX",
        //         Value = "aspmx.l.google.com",
        //         Priority = 1,
        //         Ttl = 1, // 1 = automatic
        //     });

        // new Cloudflare.Record("@", new Cloudflare.RecordArgs
        //     {
        //         Name = "@",
        //         ZoneId = zoneId,
        //         Type = "TXT",
        //         Value = "v=spf1 include:_spf.google.com ~all",
        //         Ttl = 1, // 1 = automatic
        //     });
    }
}
