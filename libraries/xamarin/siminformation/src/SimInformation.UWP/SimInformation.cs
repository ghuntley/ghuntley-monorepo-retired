// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

ï»¿using System.Collections.Generic;
using Windows.Networking.NetworkOperators;

namespace SimInformation.UWP
{
    public class SimInformation : ISimInformation
    {
        /// <summary>
        ///     Will only return details of a SINGLE simcard.
        /// </summary>
        /// <remarks>TODO: Add support for multiple sim cards. See issue #1</remarks>
        public IReadOnlyList<SimCard> GetSimCards()
        {
            var results = new List<SimCard>();

            var modem = MobileBroadbandModem.GetDefault();
            if (modem == null)
            {
                return results.AsReadOnly();
            }

            var account = modem.CurrentAccount;
            if (account == null)
            {
                return results.AsReadOnly();
            }

            var simCard = new SimCard();

            simCard.ICCID = account.CurrentDeviceInformation.SimIccId;
            simCard.IMSI = account.CurrentDeviceInformation.SubscriberId;
            simCard.MSISDN = modem.DeviceInformation.TelephoneNumbers;

            simCard.MCC = ExtractMCC(simCard.IMSI);
            simCard.MNC = ExtractMNC(simCard.IMSI);
            simCard.MSID = ExtractMSID(simCard.IMSI);

            results.Add(simCard);

            return results.AsReadOnly();
        }

        /// <summary>
        ///     Mobile Country Code(MCC) : First 3 digits of IMSI gives you MCC.
        /// </summary>
        private static string ExtractMCC(string imsi)
        {
            if (string.IsNullOrWhiteSpace(imsi)) return string.Empty;

            var operatorId = imsi.Substring(0, 5);
            var mccId = operatorId.Substring(0, 3);

            return mccId;
            ;
        }

        /// <summary>
        ///     Mobile Network Code (MNC) : Next 2 or 3 digits give you this info.
        /// </summary>
        private static string ExtractMNC(string imsi)
        {
            if (string.IsNullOrWhiteSpace(imsi)) return string.Empty;
            var operatorId = imsi.Substring(0, 5);
            var mncId = operatorId.Substring(3, 2);

            return mncId;
        }

        /// <summary>
        ///     Mobile Station ID (MSID) : Rest of the digits. Gives away the network you are using like IS-95, TDMA , GSM etc.
        /// </summary>
        private static string ExtractMSID(string imsi)
        {
            if (string.IsNullOrWhiteSpace(imsi)) return string.Empty;

            var msid = imsi.Substring(6);

            return msid;
        }
    }
}
