# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

import exceptions
from collections import defaultdict
from string import Template

import requests

USERNAME = ""
PASSWORD = ""

USER_AGENT = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:81.0) Gecko/20100101 Firefox/81.0"
)

LOGIN_URL = "https://www.my.telstra.com.au/myaccount/home"
LOGOUT_URL = "https://www.my.telstra.com.au/myaccount/log-out"
SIGNON_URL = "https://signon.telstra.com.au/login"

OVERVIEW_URL = "https://www.my.telstra.com.au/myaccount/overview/v1"

MOBILE_SERVICE_DATA_USAGE_URL = Template(
    "https://www.my.telstra.com.au/myaccount/postpaid-mobile-data-balance.json?accountId=$accountId&serviceId=$serviceId"
)


class TelstraApi(object):
    def __init__(self, username: str, password: str):

        self.session = requests.Session()
        self.session.max_redirects = 10
        self.session.headers.update({"User-Agent": USER_AGENT})

        self.username = username
        self.password = password

    def login(self) -> None:
        login_page = self.session.get(LOGIN_URL)
        login_page.raise_for_status()

        login_data = {
            "encoded": "false",
            "goto": "https://www.my.telstra.com.au/myaccount/overview",
            "gotoOnFail": "https://www.my.telstra.com.au/myaccount/home/error?flag=EMAIL",
            "gx_charset": "UTF-8",
            "password": PASSWORD,
            "username": USERNAME,
        }

        login_response = self.session.post(
            "https://signon.telstra.com.au/login", data=login_data
        )
        login_response.raise_for_status()

        if (
            "BPSESSION" not in self.session.cookies
            or "JSESSIONID" not in self.session.cookies
        ):
            raise exceptions.AuthError

    def logout(self) -> None:
        logout_page = self.session.get(LOGOUT_URL)
        logout_page.raise_for_status()

    def get_account_overview(self) -> dict:
        overview_response = self.session.get(OVERVIEW_URL)
        overview_response.raise_for_status()

        json = overview_response.json()
        self._raise_on_error(json)

        return json

    def get_internet_data_usage(
        self, hashed_account_id: str, hashed_service_id: str
    ) -> dict:
        exit(1)

    def get_shared_data_service_usage(
        self, hashed_account_id: str, hashed_service_id: str
    ) -> dict:
        exit(1)

    def get_mobile_service_usage(
        self, hashed_account_id: str, hashed_service_id: str
    ) -> dict:
        url = MOBILE_SERVICE_DATA_USAGE_URL.substitute(
            accountId=hashed_account_id, serviceId=hashed_service_id
        )

        usage_response = self.session.get(url)
        usage_response.raise_for_status()

        json = usage_response.json()
        self._raise_on_error(json)

        return json

    def _raise_on_error(self, json) -> None:
        if "SUCCESS" not in json["status"] or json["error"] is not None:
            raise exceptions.ApiError


class Telstra(object):
    def __init__(self, username: str, password: str):

        self.telstra_api = TelstraApi(username, password)

    def services(self) -> list:
        services = defaultdict(list)

        # for service in self.get_internet_services():
        #     services["InternetServices"].append(service)

        for service in self.get_shared_data_services():
            services["SharedDataServices"].append(service)

        for service in self.get_mobile_services():
            services["MobileServices"].append(service)

        return services

    def get_internet_services(self) -> list:
        raise NotImplementedError()

    def get_mobile_services(self) -> list:
        services = []

        self.telstra_api.login()

        account_overview_json = self.telstra_api.get_account_overview()

        for service in account_overview_json["result"]["mobileServiceLinks"]:

            data_usage_json = self.telstra_api.get_mobile_service_usage(
                service["hashedAccountId"], service["hashedServiceId"]
            )
            data_usage = data_usage_json["result"]["internet_usage"]

            services.append(
                {
                    "DataUsage": {
                        # nb. Telstra uses 1GB (Gigabyte) = 1,000MB (Megabytes)
                        "DataUsedInMb": data_usage["unformattedDataUsed"] / 1000,
                        "DataUsedInGb": data_usage["unformattedDataUsed"] / 1000 / 1000,
                        "DataAllowanceInMb": data_usage["unformattedDataAllowance"]
                        / 1000,
                        "DataAllowanceInGb": data_usage["unformattedDataAllowance"]
                        / 1000
                        / 1000,
                        "DaysRemaining": data_usage["daysRemaining"],
                        "CycleStartDate": data_usage["startDate"],
                        "CycleEndDate": data_usage["endDate"],
                        "CycleNextStartDate": data_usage[
                            "nextCycleStartDateWithTimezone"
                        ],
                        "PAYGCharge": data_usage["paygCharge"],
                        "PercentageDataUsed": data_usage["percentagedDataUsed"],
                        "PercentageDataRemaining": data_usage[
                            "percentageDataRemaining"
                        ],
                        "PercentageDaysPassed": data_usage["percentageDaysPassed"],
                        "Shaped": data_usage["shaped"],
                        "ShapedReason": data_usage["usageShapedReason"],
                        "RoamingDataUsed": data_usage["roamingDataUsed"],
                    },
                    "DataRoamingEnabled": service["isMobileDataRoamingActivated"],
                    "HashedAccountId": service["hashedAccountId"],
                    "HashedServiceId": service["hashedServiceId"],
                    "Name": service["serviceIdentifierValue"],
                    "NickName": service["nickName"],
                    "PlanName": service["planName"],
                    "ProductCategory": service["productCategory"],
                    "ServiceType": service["serviceType"],
                    "SuspendedService": service["suspendedService"],
                    "UnlimitedPlan": service["unlimitedPlan"],
                }
            )

        self.telstra_api.logout()

        return services

    def get_shared_data_services(self) -> list:
        services = []

        self.telstra_api.login()

        account_overview_json = self.telstra_api.get_account_overview()

        for service in account_overview_json["result"]["sharedDataServiceLinks"]:

            services.append(
                {
                    "DataRoamingEnabled": service["isMobileDataRoamingActivated"],
                    "HashedAccountId": service["hashedAccountId"],
                    "HashedServiceId": service["hashedServiceId"],
                    "Name": service["serviceIdentifierValue"],
                    "NickName": service["nickName"],
                    "PlanName": service["planName"],
                    "ProductCategory": service["productCategory"],
                    "ServiceType": service["serviceType"],
                    "SuspendedService": service["suspendedService"],
                    "UnlimitedPlan": service["unlimitedPlan"],
                }
            )

        self.telstra_api.logout()

        return services
