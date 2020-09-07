// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

ï»¿using System;
using System.Collections.Generic;
using CoreTelephony;

namespace SimInformation.iOS
{
    public class SimInformation : ISimInformation
    {
        private readonly CTTelephonyNetworkInfo _telephonyNetworkInfo;

        public SimInformation(CTTelephonyNetworkInfo telephonyNetworkInfo = null)
        {
            _telephonyNetworkInfo = telephonyNetworkInfo ?? new CTTelephonyNetworkInfo();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public IReadOnlyList<SimCard> GetSimCards()
        {
            var results = new List<SimCard>();

            var simCard = new SimCard();
            simCard.MNC = _telephonyNetworkInfo.SubscriberCellularProvider.MobileNetworkCode;
            simCard.MCC = _telephonyNetworkInfo.SubscriberCellularProvider.MobileCountryCode;

            results.Add(simCard);

            return results.AsReadOnly();
        }
    }
}
