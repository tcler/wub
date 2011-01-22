# 7.2.  Normalization

# The end user's input MUST be normalized into an Identifier, as follows:

# If the user's input starts with the "xri://" prefix, it MUST be stripped off, so that XRIs are used in the canonical form.
# If the first character of the resulting string is an XRI Global Context Symbol ("=", "@", "+", "$", "!") or "(", as defined in Section 2.2.1 of [XRI_Syntax_2.0], then the input SHOULD be treated as an XRI.
# Otherwise, the input SHOULD be treated as an http URL; if it does not include a "http" or "https" scheme, the Identifier MUST be prefixed with the string "http://". If the URL contains a fragment part, it MUST be stripped off together with the fragment delimiter character "#". See Section 11.5.2 for more information.
# URL Identifiers MUST then be further normalized by both following redirects when retrieving their content and finally applying the rules in Section 6 of [RFC3986] to the final destination URL. This final URL MUST be noted by the Relying Party as the Claimed Identifier and be used when requesting authentication.

namespace eval OpenId {
    export normalize
}