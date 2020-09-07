# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

"""telstra_data_usage - retrieves mobile and fixed services data usage for Telstra services"""

# Set default logging handler to avoid "No handler found" warnings.
import logging
from logging import NullHandler

from .__version__ import __version__

logging.getLogger(__name__).addHandler(NullHandler())
