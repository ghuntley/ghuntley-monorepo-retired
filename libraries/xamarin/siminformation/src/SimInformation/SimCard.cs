// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

ï»¿using System.Collections.Generic;

namespace SimInformation
{
    public class SimCard
    {
        public SimCard()
        {
            MSISDN = new List<string>();
        }

        /// <summary>
        ///     Integrated Circuit Card ID.
        /// </summary>
        /// <remarks>Integrated Circuit Card Identifier identifies each SIM internationally.</remarks>
        public string ICCID { get; set; }

        /// <summary>
        ///     Mobile Country Code
        /// </summary>
        /// <remarks>Identifies the country of origin by a two- or three-digit code.</remarks>
        public string MCC { get; set; }

        /// <summary>
        ///     International Mobile Subscriber Identity.
        /// </summary>
        /// <remarks>
        ///     International mobile Subscriber Identity is used to identify the user of a cellular network and is a unique
        ///     identification associated with all cellular networks. It is stored as a 64 bit field and is sent by the phone to
        ///     the network. It is also used for acquiring other details of the mobile in the HLR or as locally copied in the
        ///     Visitor Location Register. It consists of three parts:
        ///     - Mobile Country Code(MCC) : First 3 digits of IMSI gives you MCC.
        ///     - Mobile Network Code (MNC) : Next 2 or 3 digits give you this info.
        ///     - Mobile Station ID (MSID) : Rest of the digits. Gives away the network you are using like IS-95, TDMA , GSM etc.
        /// </remarks>
        public string IMSI { get; set; }

        /// <summary>
        ///     Mobile Station ID.
        /// </summary>
        /// <remarks>The network you are using like IS-95, TDMA , GSM etc.</remarks>
        public string MSID { get; set; }

        /// <summary>
        ///     Mobile Network Code.
        /// </summary>
        public string MNC { get; set; }

        /// <summary>
        ///     Mobile Subscriber International ISDN Number.
        /// </summary>
        /// <remarks>
        ///     Mobile Station ISDN number is the full phone number of a subscriber, including the national country code (e.g.
        ///     1 for US, 44 for UK, etc.). The purpose of the MSISDN is simply to allow a device to be called. A subscriber can
        ///     have multiple MSISDNs (e.g. one phone number for business, one for personal calls, one for fax, etc.), but
        ///     generally only one IMSI. The MSISDN does not need to be stored on the SIM card. In cases where it is stored on the
        ///     SIM, the main reason is so that the user can use check to see what their own MSISDN is (in case they forget). The
        ///     MSISDN is never signaled to of from the device.
        /// </remarks>
        public IReadOnlyList<string> MSISDN { get; set; }
    }
}
