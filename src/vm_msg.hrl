
-define(CMD_START, 1).
-define(CMD_BUY, 2).
-define(CMD_ACCEPT, 3).
-define(CMD_CANCEL, 4).
-define(CMD_BALANCE, 5).
-define(CMD_PUT, 8).

-record(vmReq, {cmd, clientId, cardNumber, cardType, tId, product, money}).



