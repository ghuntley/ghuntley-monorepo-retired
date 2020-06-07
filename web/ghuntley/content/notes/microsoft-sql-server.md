---
title: Microsoft SQL Server
layout: notes
tags:
 - mssql
---

Microsoft SQL server is more than a database. It's actually a suite of data applications:

1. Database engine (which is what you love/hate)
1. Service Broker (the original social media application)
1. Replication (it's my data but I will update you on anything new or any updates)
1. SQL CMD (DBAs .. please stand up)
1. SSIS (which enables you to do evil stuff like suck data out of DB2 and dump it into ORACLE)
1. SSAS Multi-dimensional (a powerful aggregation monster that is very complicated)
1. SSAS Tabular (a powerful in-memory aggregation monster that is easier to develop than it's cousin)
1. SSRS (enough said)
1. MDS (a very big dunny brush that cleans data and builds master lists of ....)
1. DQS (a very small dunny brush)
1. CDC (an interesting way to manage changes in data)
1. Profiler (I will slow down the database engine so that I can find out what is slowing down the database engine)
1. Distributed Relay (good luck)
1. Database Tuning Advisor (spamware that should never be used)
1. Machine Learning Server (tell me what I am now thinking about?)
1. SSMS (the worlds best and worst ide)
1. BIDS (a betting shop for SSIS, SSAS, and SSRS)
1. SSDT (BIDS's better looking sibling)
1. mssql -cli (I am new and 1980s looking)
1. Operation Studio (OSX and Linux users now have access to dumbed down SSMS)

Azure SQL database and Azure SQL Data Warehouse are a different kettle of fish altogether. Well .. the Azure SQL database engine comes from the same branch of code as the SQL Server database engine (feature switching at it's best).


# cheat sheet

## version of SQL Server

```
select @@version
Microsoft SQL Server 2008 (SP1) - 10.0.2531.0 (X64)   Mar 29 2009 
10:11:52   Copyright (c) 1988-2008 Microsoft Corporation  Express 
Edition (64-bit) on Windows NT 6.1 <X64> (Build 7600: )
```
