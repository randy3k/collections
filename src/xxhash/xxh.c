#include "xxh.h"

// much of the following is derived from the fastdigest package but adapt to xxh


// https://johnnylee-sde.github.io/Fast-unsigned-integer-to-hex-string/
int uint64ToHex(uint64_t num, char *s)
{
    static const char digits[513] =
        "000102030405060708090a0b0c0d0e0f"
        "101112131415161718191a1b1c1d1e1f"
        "202122232425262728292a2b2c2d2e2f"
        "303132333435363738393a3b3c3d3e3f"
        "404142434445464748494a4b4c4d4e4f"
        "505152535455565758595a5b5c5d5e5f"
        "606162636465666768696a6b6c6d6e6f"
        "707172737475767778797a7b7c7d7e7f"
        "808182838485868788898a8b8c8d8e8f"
        "909192939495969798999a9b9c9d9e9f"
        "a0a1a2a3a4a5a6a7a8a9aaabacadaeaf"
        "b0b1b2b3b4b5b6b7b8b9babbbcbdbebf"
        "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf"
        "d0d1d2d3d4d5d6d7d8d9dadbdcdddedf"
        "e0e1e2e3e4e5e6e7e8e9eaebecedeeef"
        "f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff";

    uint32_t x = (uint32_t)num;
    int i = 3;
    char *lut = (char *) digits;
    while (i >= 0)
    {
        int pos = (x & 0xFF) * 2;
        char ch = lut[pos];
        s[i * 2] = ch;

        ch = lut[pos + 1];
        s[i * 2 + 1] = ch;

        x >>= 8;
        i -= 1;
    }

    return 0;
}

static void OutChar(R_outpstream_t stream, int c)
{
    XXH3_state_t* const xxh_state = (XXH3_state_t* const) stream->data;
    XXH3_64bits_update(xxh_state, &c, sizeof(int));
}

static void OutBytes(R_outpstream_t stream, void *buf, int length)
{
    XXH3_state_t* const xxh_state = (XXH3_state_t* const) stream->data;
    XXH3_64bits_update(xxh_state, buf, length);
}


const char* xxh_digest(SEXP x) {
    XXH3_state_t* const xxh_state = XXH3_createState();
    XXH3_64bits_reset_withSeed(xxh_state, 0);
    struct R_outpstream_st stream;
    R_pstream_format_t type = R_pstream_binary_format;
    int version = 0;

    R_InitOutPStream(&stream, (R_pstream_data_t) xxh_state, type, version,
                 OutChar, OutBytes, NULL, R_NilValue);

    R_Serialize(x, &stream);

    XXH64_hash_t hash = XXH3_64bits_digest(xxh_state);
    char* s = R_alloc(sizeof(char), 9);
    uint64ToHex(hash, s);
    s[8] = '\x00';
    return s;
}
