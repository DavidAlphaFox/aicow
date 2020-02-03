%% @doc Eliptic curve digital signature algorithm
%%
%% Helper functions for encoding/decoding ECDSA signature
%%
%% @end
-module(aicow_jwt_ecdsa).

-include_lib("public_key/include/public_key.hrl").

-export([
    signature/1,
    signature/3
]).

-define(EC_GROUP_DEGREE, #{
    sect571r1 => 571,
    sect571k1 => 571,
    sect409r1 => 409,
    sect409k1 => 409,
    secp521r1 => 521,
    secp384r1 => 384,
    secp224r1 => 224,
    secp224k1 => 224,
    secp192k1 => 192,
    secp160r2 => 160,
    secp128r2 => 128,
    secp128r1 => 128,
    sect233r1 => 233,
    sect233k1 => 233,
    sect193r2 => 193,
    sect193r1 => 193,
    sect131r2 => 131,
    sect131r1 => 131,
    sect283r1 => 283,
    sect283k1 => 283,
    sect163r2 => 163,
    secp256k1 => 256,
    secp160k1 => 160,
    secp160r1 => 160,
    secp112r2 => 112,
    secp112r1 => 112,
    sect113r2 => 113,
    sect113r1 => 113,
    sect239k1 => 239,
    sect163r1 => 163,
    sect163k1 => 163,
    secp256r1 => 256,
    secp192r1 => 192,
    brainpoolP160r1 => 160,
    brainpoolP160t1 => 160,
    brainpoolP192r1 => 192,
    brainpoolP192t1 => 192,
    brainpoolP224r1 => 224,
    brainpoolP224t1 => 224,
    brainpoolP256r1 => 256,
    brainpoolP256t1 => 256,
    brainpoolP320r1 => 320,
    brainpoolP320t1 => 320,
    brainpoolP384r1 => 384,
    brainpoolP384t1 => 384,
    brainpoolP512r1 => 512,
    brainpoolP512t1 => 512
}).
%% @doc Signature for JWT verification
%%
%% Transcode the ECDSA Base64-encoded signature into ASN.1/DER format
%%
%% @end
signature(Base64Sig) ->
    Signature = ai_base64:decode(Base64Sig,#{url => true}),
    SignatureLen = byte_size(Signature),
    {RBin, SBin} = split_binary(Signature, (SignatureLen div 2)),
    R = crypto:bytes_to_integer(RBin),
    S = crypto:bytes_to_integer(SBin),
    public_key:der_encode('ECDSA-Sig-Value', #'ECDSA-Sig-Value'{ r = R, s = S }).

%% @doc Signature to sign JWT
%%
%% Transcodes the JCA ASN.1/DER-encoded signature into the concatenated R + S format
%% a.k.a <em>raw</em> format
%%
%% @end
signature(Payload, Crypto, Key) ->
    Der = public_key:sign(Payload, Crypto, Key),
    raw(Der, Key).

raw(Der, #'ECPrivateKey'{parameters = {namedCurve, NamedCurve}}) ->
    #'ECDSA-Sig-Value'{ r = R, s = S } = public_key:der_decode('ECDSA-Sig-Value', Der),
    CurveName = pubkey_cert_records:namedCurves(NamedCurve),
    GroupDegree = group_degree(CurveName),
    Size = (GroupDegree + 7) div 8,
    RBin = int_to_bin(R),
    SBin = int_to_bin(S),
    RPad = pad(RBin, Size),
    SPad = pad(SBin, Size),
    <<RPad/binary, SPad/binary>>.

%% @private
int_to_bin(X) when X < 0 ->
    int_to_bin_neg(X, []);
int_to_bin(X) ->
    int_to_bin_pos(X, []).

%% @private
int_to_bin_pos(0, Ds = [_|_]) ->
    list_to_binary(Ds);
int_to_bin_pos(X, Ds) ->
    int_to_bin_pos(X bsr 8, [(X band 255)|Ds]).

%% @private
int_to_bin_neg(-1, Ds = [MSB|_]) when MSB >= 16#80 ->
    list_to_binary(Ds);
int_to_bin_neg(X, Ds) ->
    int_to_bin_neg(X bsr 8, [(X band 255)|Ds]).

%% @private
pad(Bin, Size) when byte_size(Bin) =:= Size ->
    Bin;
pad(Bin, Size) when byte_size(Bin) < Size ->
    pad(<<0, Bin/binary>>, Size).

%% See the OpenSSL documentation for EC_GROUP_get_degree()
group_degree(CurveName) ->
    maps:get(CurveName, ?EC_GROUP_DEGREE).
