package main

import (
	cloudflare "github.com/pulumi/pulumi-cloudflare/sdk/v2/go/cloudflare"
	"github.com/pulumi/pulumi/sdk/v2/go/pulumi"
)

func ghuntley_dev(ctx *pulumi.Context) {

	const zoneName = "ghuntley-dev"
	const zoneId = "a548e719763b08ac695475735536d9ad"

	// domain ownership verification

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
