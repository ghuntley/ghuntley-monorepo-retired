class BaseError(Exception):
    """
    Base exception class for exceptions.
    """


class AuthError(BaseError):
    """
    Invalid authentication details.
    """

    def __init__(self):
        super(AuthError, self).__init__("Invalid authentication details")


class ApiError(BaseError):
    """
    Unexpected response from the Telstra API.
    """

    def __init__(self):
        super(AuthError, self).__init__("Unexpected response from the API")
