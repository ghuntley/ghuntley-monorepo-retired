using Pulumi;
using Pulumi.Cloudflare;

class MyStack : Stack
{
    public MyStack()
    {
        const string zoneId = "8cbecb7f14bf65e2e7060b325d928340";

         var record = new Cloudflare.Record("sample-record", new Cloudflare.RecordArgs
            {
                Name = "my-record",
                ZoneId = zoneId,
                Type = "A",
                Value = "192.168.0.11",
                Ttl = 3600,
            });
    }

}
