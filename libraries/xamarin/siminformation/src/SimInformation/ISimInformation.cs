// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

ï»¿using System.Collections.Generic;

namespace SimInformation
{
    public interface ISimInformation
    {
        IReadOnlyList<SimCard> GetSimCards();
    }
}
