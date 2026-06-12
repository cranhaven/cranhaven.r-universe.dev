/* Declarations for the UTS #39 / UAX #24 data tables.
 *
 * GENERATED FILE -- DO NOT EDIT.
 * Regenerate with `ninja regen-uts39` in the build dir.
 *
 * Source: UTS #39 17.0.0 (Unicode Security Mechanisms) and
 *         UAX #24 (Script_Property, Script_Extensions).
 * Input files vendored under packages/utf8proc/data/.
 */

#ifndef UTS39_DATA_H_INCLUDED
#define UTS39_DATA_H_INCLUDED

#include <stdint.h>
#include <stddef.h>

#define UTS39_UNICODE_VERSION "17.0.0"

#define UTS39_IDTYPE_COUNT 11
#define UTS39_IDTYPE_DEFAULT_IGNORABLE 0
#define UTS39_IDTYPE_DEPRECATED 1
#define UTS39_IDTYPE_EXCLUSION 2
#define UTS39_IDTYPE_INCLUSION 3
#define UTS39_IDTYPE_LIMITED_USE 4
#define UTS39_IDTYPE_NOT_NFKC 5
#define UTS39_IDTYPE_NOT_XID 6
#define UTS39_IDTYPE_OBSOLETE 7
#define UTS39_IDTYPE_RECOMMENDED 8
#define UTS39_IDTYPE_TECHNICAL 9
#define UTS39_IDTYPE_UNCOMMON_USE 10

#define UTS39_ID_ALLOWED    1
#define UTS39_ID_RESTRICTED 0

typedef enum {
    UTS39_RL_ASCII_ONLY = 0,
    UTS39_RL_SINGLE_SCRIPT,
    UTS39_RL_HIGHLY_RESTRICTIVE,
    UTS39_RL_MODERATELY_RESTRICTIVE,
    UTS39_RL_MINIMALLY_RESTRICTIVE,
    UTS39_RL_UNRESTRICTED
} uts39_restriction_level_t;

#define UTS39_SCRIPT_COUNT 179
#define UTS39_SC_Zyyy 0
#define UTS39_SC_Zinh 1
#define UTS39_SC_Adlm 2
#define UTS39_SC_Aghb 3
#define UTS39_SC_Ahom 4
#define UTS39_SC_Arab 5
#define UTS39_SC_Armi 6
#define UTS39_SC_Armn 7
#define UTS39_SC_Avst 8
#define UTS39_SC_Bali 9
#define UTS39_SC_Bamu 10
#define UTS39_SC_Bass 11
#define UTS39_SC_Batk 12
#define UTS39_SC_Beng 13
#define UTS39_SC_Berf 14
#define UTS39_SC_Bhks 15
#define UTS39_SC_Bopo 16
#define UTS39_SC_Brah 17
#define UTS39_SC_Brai 18
#define UTS39_SC_Bugi 19
#define UTS39_SC_Buhd 20
#define UTS39_SC_Cakm 21
#define UTS39_SC_Cans 22
#define UTS39_SC_Cari 23
#define UTS39_SC_Cham 24
#define UTS39_SC_Cher 25
#define UTS39_SC_Chrs 26
#define UTS39_SC_Copt 27
#define UTS39_SC_Cpmn 28
#define UTS39_SC_Cprt 29
#define UTS39_SC_Cyrl 30
#define UTS39_SC_Deva 31
#define UTS39_SC_Diak 32
#define UTS39_SC_Dogr 33
#define UTS39_SC_Dsrt 34
#define UTS39_SC_Dupl 35
#define UTS39_SC_Egyp 36
#define UTS39_SC_Elba 37
#define UTS39_SC_Elym 38
#define UTS39_SC_Ethi 39
#define UTS39_SC_Gara 40
#define UTS39_SC_Geor 41
#define UTS39_SC_Glag 42
#define UTS39_SC_Gong 43
#define UTS39_SC_Gonm 44
#define UTS39_SC_Goth 45
#define UTS39_SC_Gran 46
#define UTS39_SC_Grek 47
#define UTS39_SC_Gujr 48
#define UTS39_SC_Gukh 49
#define UTS39_SC_Guru 50
#define UTS39_SC_Hanb 51
#define UTS39_SC_Hang 52
#define UTS39_SC_Hani 53
#define UTS39_SC_Hano 54
#define UTS39_SC_Hatr 55
#define UTS39_SC_Hebr 56
#define UTS39_SC_Hira 57
#define UTS39_SC_Hluw 58
#define UTS39_SC_Hmng 59
#define UTS39_SC_Hmnp 60
#define UTS39_SC_Hrkt 61
#define UTS39_SC_Hung 62
#define UTS39_SC_Ital 63
#define UTS39_SC_Java 64
#define UTS39_SC_Jpan 65
#define UTS39_SC_Kali 66
#define UTS39_SC_Kana 67
#define UTS39_SC_Kawi 68
#define UTS39_SC_Khar 69
#define UTS39_SC_Khmr 70
#define UTS39_SC_Khoj 71
#define UTS39_SC_Kits 72
#define UTS39_SC_Knda 73
#define UTS39_SC_Kore 74
#define UTS39_SC_Krai 75
#define UTS39_SC_Kthi 76
#define UTS39_SC_Lana 77
#define UTS39_SC_Laoo 78
#define UTS39_SC_Latn 79
#define UTS39_SC_Lepc 80
#define UTS39_SC_Limb 81
#define UTS39_SC_Lina 82
#define UTS39_SC_Linb 83
#define UTS39_SC_Lisu 84
#define UTS39_SC_Lyci 85
#define UTS39_SC_Lydi 86
#define UTS39_SC_Mahj 87
#define UTS39_SC_Maka 88
#define UTS39_SC_Mand 89
#define UTS39_SC_Mani 90
#define UTS39_SC_Marc 91
#define UTS39_SC_Medf 92
#define UTS39_SC_Mend 93
#define UTS39_SC_Merc 94
#define UTS39_SC_Mero 95
#define UTS39_SC_Mlym 96
#define UTS39_SC_Modi 97
#define UTS39_SC_Mong 98
#define UTS39_SC_Mroo 99
#define UTS39_SC_Mtei 100
#define UTS39_SC_Mult 101
#define UTS39_SC_Mymr 102
#define UTS39_SC_Nagm 103
#define UTS39_SC_Nand 104
#define UTS39_SC_Narb 105
#define UTS39_SC_Nbat 106
#define UTS39_SC_Newa 107
#define UTS39_SC_Nkoo 108
#define UTS39_SC_Nshu 109
#define UTS39_SC_Ogam 110
#define UTS39_SC_Olck 111
#define UTS39_SC_Onao 112
#define UTS39_SC_Orkh 113
#define UTS39_SC_Orya 114
#define UTS39_SC_Osge 115
#define UTS39_SC_Osma 116
#define UTS39_SC_Ougr 117
#define UTS39_SC_Palm 118
#define UTS39_SC_Pauc 119
#define UTS39_SC_Perm 120
#define UTS39_SC_Phag 121
#define UTS39_SC_Phli 122
#define UTS39_SC_Phlp 123
#define UTS39_SC_Phnx 124
#define UTS39_SC_Plrd 125
#define UTS39_SC_Prti 126
#define UTS39_SC_Rjng 127
#define UTS39_SC_Rohg 128
#define UTS39_SC_Runr 129
#define UTS39_SC_Samr 130
#define UTS39_SC_Sarb 131
#define UTS39_SC_Saur 132
#define UTS39_SC_Sgnw 133
#define UTS39_SC_Shaw 134
#define UTS39_SC_Shrd 135
#define UTS39_SC_Sidd 136
#define UTS39_SC_Sidt 137
#define UTS39_SC_Sind 138
#define UTS39_SC_Sinh 139
#define UTS39_SC_Sogd 140
#define UTS39_SC_Sogo 141
#define UTS39_SC_Sora 142
#define UTS39_SC_Soyo 143
#define UTS39_SC_Sund 144
#define UTS39_SC_Sunu 145
#define UTS39_SC_Sylo 146
#define UTS39_SC_Syrc 147
#define UTS39_SC_Tagb 148
#define UTS39_SC_Takr 149
#define UTS39_SC_Tale 150
#define UTS39_SC_Talu 151
#define UTS39_SC_Taml 152
#define UTS39_SC_Tang 153
#define UTS39_SC_Tavt 154
#define UTS39_SC_Tayo 155
#define UTS39_SC_Telu 156
#define UTS39_SC_Tfng 157
#define UTS39_SC_Tglg 158
#define UTS39_SC_Thaa 159
#define UTS39_SC_Thai 160
#define UTS39_SC_Tibt 161
#define UTS39_SC_Tirh 162
#define UTS39_SC_Tnsa 163
#define UTS39_SC_Todr 164
#define UTS39_SC_Tols 165
#define UTS39_SC_Toto 166
#define UTS39_SC_Tutg 167
#define UTS39_SC_Ugar 168
#define UTS39_SC_Vaii 169
#define UTS39_SC_Vith 170
#define UTS39_SC_Wara 171
#define UTS39_SC_Wcho 172
#define UTS39_SC_Xpeo 173
#define UTS39_SC_Xsux 174
#define UTS39_SC_Yezi 175
#define UTS39_SC_Yiii 176
#define UTS39_SC_Zanb 177
#define UTS39_SC_Zzzz 178

#define UTS39_SCRIPT_BITSET_WORDS 3

/* A range covers start..start+len-1; value is the lookup payload */
typedef struct {
    uint32_t start;
    uint16_t len;
    uint16_t value;
} uts39_range_t;

extern const char *const uts39_script_short[UTS39_SCRIPT_COUNT];
extern const char *const uts39_script_long [UTS39_SCRIPT_COUNT];

extern const uts39_range_t uts39_script_ranges[];
extern const size_t        uts39_script_ranges_count;

extern const uts39_range_t uts39_scx_ranges[];
extern const size_t        uts39_scx_ranges_count;
extern const uint64_t      uts39_scx_sets[][UTS39_SCRIPT_BITSET_WORDS];
extern const size_t        uts39_scx_sets_count;

extern const uts39_range_t uts39_idstatus_ranges[];
extern const size_t        uts39_idstatus_ranges_count;

extern const uts39_range_t uts39_idtype_ranges[];
extern const size_t        uts39_idtype_ranges_count;

typedef struct {
    uint32_t src;
    uint32_t offset;
    uint16_t length;
} uts39_skeleton_entry_t;

extern const uts39_skeleton_entry_t uts39_skeleton_entries[];
extern const size_t                 uts39_skeleton_entries_count;
extern const uint32_t               uts39_skeleton_chars[];

typedef struct { uint32_t a, b; } uts39_pair_t;
extern const uts39_pair_t uts39_intentional[];
extern const size_t       uts39_intentional_count;

#endif
