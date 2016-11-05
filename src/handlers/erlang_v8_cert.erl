-module(erlang_v8_cert).

-export([run/2]).

-include_lib("public_key/include/public_key.hrl").

run([<<"validity">>, Hostname, Port], _HandlerContext) ->
    application:ensure_all_started(ssl),
    case ssl:connect(binary_to_list(Hostname), Port,  [], 10000) of
        {ok, Socket} ->
            case ssl:peercert(Socket) of
                {ok, Cert} ->
                    OTPCert  = public_key:pkix_decode_cert(Cert, otp),
                    TBSCert = OTPCert#'OTPCertificate'.tbsCertificate,

                    {'Validity', NotBefore, NotAfter}
                        = TBSCert#'OTPTBSCertificate'.validity,

                    UTime = calendar:universal_time(),
                    Now = calendar:datetime_to_gregorian_seconds(UTime),

                    NotBeforeSeconds
                        = pubkey_cert:time_str_2_gregorian_sec(NotBefore) - Now,
                    NotAfterSeconds
                        = pubkey_cert:time_str_2_gregorian_sec(NotAfter) - Now,

                    {ok, #{
                       'validInSeconds' => NotBeforeSeconds,
                       'invalidInSeconds' => NotAfterSeconds
                    }};
                {error, no_peercert} ->
                    {error, <<"No peer certificate.">>}
            end;
        {error, {tls_alert, "record overflow"}} ->
            {error, <<"Invalid TLS host and port combination.">>};
        Error = {error, _} ->
            Error
    end.
