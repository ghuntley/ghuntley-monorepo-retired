// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

using Pulumi;
using Pulumi.Cloudflare;

partial class Infrastructure : Stack
{
    public void CloudflareDNS()
    {
        const string zoneId = "8cbecb7f14bf65e2e7060b325d928340";

        #region hosts

        new Record("dns-record-root", new RecordArgs
        {
            Name = "@",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-devenv-public", new RecordArgs
        {
            Name = "public.devenv",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "devenv.australiaeast.cloudapp.azure.com",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-esxi", new RecordArgs
        {
            Name = "esxi",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-git", new RecordArgs
        {
            Name = "git",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-ide", new RecordArgs
        {
            Name = "ide",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-influxdb", new RecordArgs
        {
            Name = "influxdb",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });


        new Record("dns-record-motion", new RecordArgs
        {
            Name = "motion",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-nexus", new RecordArgs
        {
            Name = "nexus",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-proxy", new RecordArgs
        {
            Name = "proxy",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-smtp", new RecordArgs
        {
            Name = "smtp",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-unifi", new RecordArgs
        {
            Name = "unifi",
            ZoneId = zoneId,
            Type = "CNAME",
            Value = "compute.wg.ghuntley.net",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-devenv", new RecordArgs
        {
            Name = "devenv.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.74.246.124",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-compute", new RecordArgs
        {
            Name = "compute.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.104.42.8",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-lenovo", new RecordArgs
        {
            Name = "lenovo.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.120.133.16",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-pihole", new RecordArgs
        {
            Name = "pihole.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.94.185.64";
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-speedify", new RecordArgs
        {
            Name = "speedify.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.119.53.67";
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-surfacego", new RecordArgs
        {
            Name = "surfacego.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.77.47.105",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-pixel4xl", new RecordArgs
        {
            Name = "pixel4xl.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.123.145.101",
            Ttl = 1, // 1 = automatic
        });

        new Record("dns-record-wg-metabox", new RecordArgs
        {
            Name = "metabox.wg",
            ZoneId = zoneId,
            Type = "A",
            Value = "100.64.69.62",
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

