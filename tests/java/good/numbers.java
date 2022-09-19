public class numbers {
    int i1 = 0;
    int i2 = 1;
    int i3 = 0xAF01;
    double d1 = .3141;
    double d2 = 3.1534;
    double d3 = 3.1534D;

    int literal_octal = 052;
    int literal_decimal = 42;
    int literal_hex = 0x2A;

    long creditCardNumber = 1234_5678_9012_3456L;
    long socialSecurityNumber = 999_99_9999L;
    float pi = 	3.14_15F;
    long hexBytes = 0xFF_EC_DE_5E;
    long hexWords = 0xCAFE_BABE;
    long maxLong = 0x7fff_ffff_ffff_ffffL;

    // Parsing bytes is not supported
    // byte nybbles = 0b0010_0101;
    // long bytes = 0b11010010_01101001_10010100_10010010;
}
