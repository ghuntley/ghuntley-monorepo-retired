package main

import (
	cloudflare "github.com/pulumi/pulumi-cloudflare/sdk/v2/go/cloudflare"
	"github.com/pulumi/pulumi/sdk/v2/go/pulumi"
)

func geoffreyhuntley_com(ctx *pulumi.Context) {

	const zoneName = "geoffreyhuntley-com"
	const zoneId = "7823ac7e890b6b2fc0e8da4f16c999a4"

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-root", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("@"),
		Type:   pulumi.String("CNAME"),
		Value:  pulumi.String("ghuntley.netlify.com"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-www", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("www"),
		Type:   pulumi.String("CNAME"),
		Value:  pulumi.String("ghuntley.netlify.com"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-git", &cloudflare.RecordArgs{
		ZoneId:  pulumi.String(zoneId),
		Name:    pulumi.String("git"),
		Type:    pulumi.String("CNAME"),
		Value:   pulumi.String("github.com"),
		Proxied: pulumi.Bool(true),
		Ttl:     pulumi.Int(1),
	})

	// domain ownership verification

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-google-site-verification", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("@"),
		Type:   pulumi.String("TXT"),
		Value:  pulumi.String("google-site-verification=Ygi199GbO9ipejg0519FZUH5JVbWB_qIS9JOke_ulp8"),
		Ttl:    pulumi.Int(1),
	})

	// email

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-dkim", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("google._domainkey"),
		Type:   pulumi.String("TXT"),
		Value:  pulumi.String("v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCgoXN71FIF9uJQI7jcD6OwsUXtgwqqtn9aK+cFekpP995w6haBNEI7gQPPEN4fiyvYJ0j6GmqMzOAdQNqseXYS2fEg7D2doZxNu6cA/VNd06XvhIKDvrKsdTkan9IgHuE3Ocrh/zMYNML4o4KvuAdb7ryGBTah7EEf5//MlWREpQIDAQAB"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-spf", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("@"),
		Type:   pulumi.String("TXT"),
		Value:  pulumi.String("v=spf1 include:_spf.google.com ~all"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-mx-1", &cloudflare.RecordArgs{
		ZoneId:   pulumi.String(zoneId),
		Name:     pulumi.String("@"),
		Type:     pulumi.String("MX"),
		Value:    pulumi.String("aspmx.l.google.com"),
		Priority: pulumi.Int(1),
		Ttl:      pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-mx-5-1", &cloudflare.RecordArgs{
		ZoneId:   pulumi.String(zoneId),
		Name:     pulumi.String("@"),
		Type:     pulumi.String("MX"),
		Value:    pulumi.String("alt1.aspmx.l.google.com"),
		Priority: pulumi.Int(5),
		Ttl:      pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-mx-5-2", &cloudflare.RecordArgs{
		ZoneId:   pulumi.String(zoneId),
		Name:     pulumi.String("@"),
		Type:     pulumi.String("MX"),
		Value:    pulumi.String("alt2.aspmx.l.google.com"),
		Priority: pulumi.Int(5),
		Ttl:      pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-mx-10-1", &cloudflare.RecordArgs{
		ZoneId:   pulumi.String(zoneId),
		Name:     pulumi.String("@"),
		Type:     pulumi.String("MX"),
		Value:    pulumi.String("alt3.aspmx.l.google.com"),
		Priority: pulumi.Int(10),
		Ttl:      pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-mx-10-2", &cloudflare.RecordArgs{
		ZoneId:   pulumi.String(zoneId),
		Name:     pulumi.String("@"),
		Type:     pulumi.String("MX"),
		Value:    pulumi.String("alt4.aspmx.l.google.com"),
		Priority: pulumi.Int(10),
		Ttl:      pulumi.Int(1),
	})
}
