// Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
// SPDX-License-Identifier: Proprietary

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace pbpaste
{
    class Program
    {
        [STAThread]
        static void Main(string[] args)
        {
            var pasteboard = Clipboard.GetText();

            Console.WriteLine(pasteboard);
        }
    }
}

