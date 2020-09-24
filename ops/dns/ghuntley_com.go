package main

import (
	cloudflare "github.com/pulumi/pulumi-cloudflare/sdk/v2/go/cloudflare"
	"github.com/pulumi/pulumi/sdk/v2/go/pulumi"
)

func ghuntley_com(ctx *pulumi.Context) {

	const zoneName = "ghuntley-com"
	const zoneId = "9013e73e470b50749a8c7dbbc09da6b1"

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

	// google apps

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-calendar", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("calendar"),
		Type:   pulumi.String("CNAME"),
		Value:  pulumi.String("ghs.google.com"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-docs", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("docs"),
		Type:   pulumi.String("CNAME"),
		Value:  pulumi.String("ghs.google.com"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-mail", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("mail"),
		Type:   pulumi.String("CNAME"),
		Value:  pulumi.String("ghs.google.com"),
		Ttl:    pulumi.Int(1),
	})

	// domain ownership verification

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-bing-verification", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("aad95f930bf299e7925ec84a9fd86245"),
		Type:   pulumi.String("CNAME"),
		Value:  pulumi.String("verify.bing.com"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-google-site-verification", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("@"),
		Type:   pulumi.String("TXT"),
		Value:  pulumi.String("google-site-verification=Ygi199GbO9ipejg0519FZUH5JVbWB_qIS9JOke_ulp8"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-keybase-verfication", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("@"),
		Type:   pulumi.String("TXT"),
		Value:  pulumi.String("keybase-site-verification=qINp7lIpBcjEroqO0EFwILvIXlf1pWb0gpAEB3OhQn8"),
		Ttl:    pulumi.Int(1),
	})

	cloudflare.NewRecord(ctx, zoneName+"-dns-record-yandex-verification", &cloudflare.RecordArgs{
		ZoneId: pulumi.String(zoneId),
		Name:   pulumi.String("@"),
		Type:   pulumi.String("TXT"),
		Value:  pulumi.String("yandex-verification: 13b44c9f1d9d0c8b"),
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
