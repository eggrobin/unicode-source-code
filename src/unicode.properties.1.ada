package Unicode.Properties is
   
   type Binary_Property is
     -- DerivedCoreProperties.txt.
     (Lowercase, Uppercase, Cased, Case_Ignorable, Changes_When_Lowercased,
      Changes_When_Uppercased, Changes_When_Titlecased, Changes_When_Casefolded,
      Changes_When_Casemapped, Alphabetic, Default_Ignorable_Code_Point,
      Grapheme_Base, Grapheme_Extend, Grapheme_Link, Math, Id_Start,
      Id_Continue, XID_Start, XID_Continue,
      -- PropList.txt.
      ASCII_Hex_Digit, Bidi_Control, Dash, Deprecated, Diacritic, Extender,
      Hex_Digit, Hyphen, Ideographic, IDS_Binary_Operator, IDS_Trinary_Operator,
      Join_Control, Logical_Order_Exception, Noncharacter_Code_Point,
      Other_Alphabetic, Other_Default_Ignorable_Code_Point,
      Other_Grapheme_Extend, Other_ID_Continue, Other_ID_Start, Other_Lowercase,
      Other_Math, Other_Uppercase, Pattern_Syntax, Pattern_White_Space,
      Prepended_Concatenation_Mark, Quotation_Mark, Radical, Regional_Indicator,
      Sentence_Terminal, Soft_Dotted, Terminal_Punctuation, Unified_Ideograph,
      Variation_Selector, White_Space,
      -- UnicodeData.txt (but we get it from DerivedBinaryProperties.txt).
      Bidi_Mirrored,
      -- emoji-data.txt
      Emoji, Emoji_Presentation, Emoji_Modifier, Emoji_Modifier_Base,
      Emoji_Component, Extended_Pictographic);

   type General_Category is (Uppercase_Letter,
                             Lowercase_Letter,
                             Titlecase_Letter,
                             Modifier_Letter,
                             Other_Letter,
                             Nonspacing_Mark,
                             Spacing_Mark,
                             Enclosing_Mark,
                             Decimal_Number,
                             Letter_Number,
                             Other_Number,
                             Connector_Punctuation,
                             Dash_Punctuation,
                             Open_Punctuation,
                             Close_Punctuation,
                             Initial_Punctuation,
                             Final_Punctuation,
                             Other_Punctuation,
                             Math_Symbol,
                             Currency_Symbol,
                             Modifier_Symbol,
                             Other_Symbol,
                             Space_Separator,
                             Line_Separator,
                             Paragraph_Separator,
                             Control,
                             Format,
                             Surrogate,
                             Private_Use,
                             Unassigned);

   package General_Category_Groupings is
      subtype Letter is General_Category range Uppercase_Letter .. Other_Letter;
      subtype Cased_Letter is Letter range Uppercase_Letter .. Titlecase_Letter;
      subtype Mark is General_Category range Nonspacing_Mark .. Enclosing_Mark;
      subtype Punctuation is General_Category range
        Connector_Punctuation .. Other_Punctuation;
      subtype Symbol is General_Category range Math_Symbol .. Other_Symbol;
      subtype Separator is General_Category range
        Space_Separator .. Paragraph_Separator;
      subtype Other is General_Category range Control .. Unassigned;
   end General_Category_Groupings;

   function Lu return General_Category renames Uppercase_Letter;
   function Ll return General_Category renames Lowercase_Letter;
   function Lt return General_Category renames Titlecase_Letter;
   function Lm return General_Category renames Modifier_Letter;
   function Lo return General_Category renames Other_Letter;
   
   function Mn return General_Category renames Nonspacing_Mark;
   function Mc return General_Category renames Spacing_Mark;
   function Me return General_Category renames Enclosing_Mark;

   function Nd return General_Category renames Decimal_Number;
   function Nl return General_Category renames Letter_Number;
   function No return General_Category renames Other_Number;

   function Pc return General_Category renames Connector_Punctuation;
   function Pd return General_Category renames Dash_Punctuation;
   function Ps return General_Category renames Open_Punctuation;
   function Pe return General_Category renames Close_Punctuation;
   function Pi return General_Category renames Initial_Punctuation;
   function Pf return General_Category renames Final_Punctuation;
   function Po return General_Category renames Other_Punctuation;

   function Sm return General_Category renames Math_Symbol;
   function Sc return General_Category renames Currency_Symbol;
   function Sk return General_Category renames Modifier_Symbol;
   function So return General_Category renames Other_Symbol;
   
   function Zs return General_Category renames Space_Separator;
   function Zl return General_Category renames Line_Separator;
   function Zp return General_Category renames Paragraph_Separator;

   function Cc return General_Category renames Control;
   function Cf return General_Category renames Format;
   function Cs return General_Category renames Surrogate;
   function Co return General_Category renames Private_Use;
   function Cn return General_Category renames Unassigned;
     
   type Bidi_Class is (-- Strong Types
                       Left_To_Right,
                       Right_To_Left,
                       Arabic_Letter,
                       -- Weak Types
                       European_Number,
                       European_Separator,
                       European_Terminator,
                       Arabic_Number,
                       Common_Separator,
                       Nonspacing_Mark,
                       Boundary_Neutral,
                       -- Neutral Types
                       Paragraph_Separator,
                       Segment_Separator,
                       White_Space,
                       Other_Neutral,
                       -- Explicit Formatting Types
                       Left_To_Right_Embedding,
                       Left_To_Right_Override,
                       Right_To_Left_Embedding,
                       Right_To_Left_Override,
                       Pop_Directional_Format,
                       Left_To_Right_Isolate,
                       Right_To_Left_Isolate,
                       First_Strong_Isolate,
                       Pop_Directional_Isolate);
   
   function L return Bidi_Class renames Left_To_Right;
   function R return Bidi_Class renames Right_To_Left;
   function AL return Bidi_Class renames Arabic_Letter;

   function EN return Bidi_Class renames European_Number;
   function ES return Bidi_Class renames European_Separator;
   function ET return Bidi_Class renames European_Terminator;
   function AN return Bidi_Class renames Arabic_Number;
   function CS return Bidi_Class renames Common_Separator;
   function NSM return Bidi_Class renames Nonspacing_Mark;
   function BN return Bidi_Class renames Boundary_Neutral;

   function B return Bidi_Class renames Paragraph_Separator;
   function S return Bidi_Class renames Segment_Separator;
   function WS return Bidi_Class renames White_Space;
   function ON return Bidi_Class renames Other_Neutral;

   function LRE return Bidi_Class renames Left_To_Right_Embedding;
   function LRO return Bidi_Class renames Left_To_Right_Override;
   function RLE return Bidi_Class renames Right_To_Left_Embedding;
   function RLO return Bidi_Class renames Right_To_Left_Override;
   function PDF return Bidi_Class renames Pop_Directional_Format;
   function LRI return Bidi_Class renames Left_To_Right_Isolate;
   function RLI return Bidi_Class renames Right_To_Left_Isolate;
   function FSI return Bidi_Class renames First_Strong_Isolate;
   function PDI return Bidi_Class renames Pop_Directional_Isolate;
        
   type Line_Break is (Ambiguous,
                       Alphabetic,
                       Break_Both,
                       Break_After,
                       Break_Before,
                       Mandatory_Break,
                       Contingent_Break,
                       Conditional_Japanese_Starter,
                       Close_Punctuation,
                       Combining_Mark,
                       Close_Parenthesis,
                       Carriage_Return,
                       E_Base,
                       E_Modifier,
                       Exclamation,
                       Glue,
                       H2,
                       H3,
                       Hebrew_Letter,
                       Hyphen,
                       Ideographic,
                       Inseparable,
                       Infix_Numeric,
                       JL,
                       JT,
                       JV,
                       Line_Feed,
                       Next_Line,
                       Nonstarter,
                       Numeric,
                       Open_Punctuation,
                       Postfix_Numeric,
                       Prefix_Numeric,
                       Quotation,
                       Regional_Indicator,
                       Complex_Context,
                       Surrogate,
                       Space,
                       Break_Symbols,
                       Word_Joiner,
                       Unknown,
                       ZWSpace,
                       ZWJ,
                       -- Proposed
                       Aksara,
                       Aksara_Prebase,
                       Aksara_Start,
                       Virama_Final,
                       Virama);

   type Script is
    (Adlam, Caucasian_Albanian, Ahom, Arabic, Imperial_Aramaic, Armenian, Avestan,
     Balinese, Bamum, Bassa_Vah, Batak, Bengali, Bhaiksuki, Bopomofo, Brahmi,
     Braille, Buginese, Buhid, Chakma, Canadian_Aboriginal, Carian, Cham,
     Cherokee, Chorasmian, Coptic, Cypro_Minoan, Cypriot, Cyrillic, Devanagari,
     Dives_Akuru, Dogra, Deseret, Duployan, Egyptian_Hieroglyphs, Elbasan,
     Elymaic, Ethiopic, Georgian, Glagolitic, Gunjala_Gondi, Masaram_Gondi,
     Gothic, Grantha, Greek, Gujarati, Gurmukhi, Hangul, Han, Hanunoo, Hatran,
     Hebrew, Hiragana, Anatolian_Hieroglyphs, Pahawh_Hmong,
     Nyiakeng_Puachue_Hmong, Katakana_Or_Hiragana, Old_Hungarian, Old_Italic,
     Javanese, Kayah_Li, Katakana, Kawi, Kharoshthi, Khmer, Khojki,
     Khitan_Small_Script, Kannada, Kaithi, Tai_Tham, Lao, Latin, Lepcha, Limbu,
     Linear_A, Linear_B, Lisu, Lycian, Lydian, Mahajani, Makasar, Mandaic,
     Manichaean, Marchen, Medefaidrin, Mende_Kikakui, Meroitic_Cursive,
     Meroitic_Hieroglyphs, Malayalam, Modi, Mongolian, Mro, Meetei_Mayek, Multani,
     Myanmar, Nag_Mundari, Nandinagari, Old_North_Arabian, Nabataean, Newa, Nko,
     Nushu, Ogham, Ol_Chiki, Old_Turkic, Oriya, Osage, Osmanya, Old_Uyghur,
     Palmyrene, Pau_Cin_Hau, Old_Permic, Phags_Pa, Inscriptional_Pahlavi,
     Psalter_Pahlavi, Phoenician, Miao, Inscriptional_Parthian, Rejang,
     Hanifi_Rohingya, Runic, Samaritan, Old_South_Arabian, Saurashtra,
     SignWriting, Shavian, Sharada, Siddham, Khudawadi, Sinhala, Sogdian,
     Old_Sogdian, Sora_Sompeng, Soyombo, Sundanese, Syloti_Nagri, Syriac,
     Tagbanwa, Takri, Tai_Le, New_Tai_Lue, Tamil, Tangut, Tai_Viet, Telugu,
     Tifinagh, Tagalog, Thaana, Thai, Tibetan, Tirhuta, Tangsa, Toto, Ugaritic,
     Vai, Vithkuqi, Warang_Citi, Wancho, Old_Persian, Cuneiform, Yezidi, Yi,
     Zanabazar_Square, Inherited, Common, Unknown);

   type Script_Alias is not null access constant Wide_Wide_String;
   Script_Aliases : constant array (Script) of Script_Alias :=
     (new Wide_Wide_String'("Adlm"),
      new Wide_Wide_String'("Aghb"),
      new Wide_Wide_String'("Ahom"),
      new Wide_Wide_String'("Arab"),
      new Wide_Wide_String'("Armi"),
      new Wide_Wide_String'("Armn"),
      new Wide_Wide_String'("Avst"),
      new Wide_Wide_String'("Bali"),
      new Wide_Wide_String'("Bamu"),
      new Wide_Wide_String'("Bass"),
      new Wide_Wide_String'("Batk"),
      new Wide_Wide_String'("Beng"),
      new Wide_Wide_String'("Bhks"),
      new Wide_Wide_String'("Bopo"),
      new Wide_Wide_String'("Brah"),
      new Wide_Wide_String'("Brai"),
      new Wide_Wide_String'("Bugi"),
      new Wide_Wide_String'("Buhd"),
      new Wide_Wide_String'("Cakm"),
      new Wide_Wide_String'("Cans"),
      new Wide_Wide_String'("Cari"),
      new Wide_Wide_String'("Cham"),
      new Wide_Wide_String'("Cher"),
      new Wide_Wide_String'("Chrs"),
      new Wide_Wide_String'("Copt"),
      new Wide_Wide_String'("Cpmn"),
      new Wide_Wide_String'("Cprt"),
      new Wide_Wide_String'("Cyrl"),
      new Wide_Wide_String'("Deva"),
      new Wide_Wide_String'("Diak"),
      new Wide_Wide_String'("Dogr"),
      new Wide_Wide_String'("Dsrt"),
      new Wide_Wide_String'("Dupl"),
      new Wide_Wide_String'("Egyp"),
      new Wide_Wide_String'("Elba"),
      new Wide_Wide_String'("Elym"),
      new Wide_Wide_String'("Ethi"),
      new Wide_Wide_String'("Geor"),
      new Wide_Wide_String'("Glag"),
      new Wide_Wide_String'("Gong"),
      new Wide_Wide_String'("Gonm"),
      new Wide_Wide_String'("Goth"),
      new Wide_Wide_String'("Gran"),
      new Wide_Wide_String'("Grek"),
      new Wide_Wide_String'("Gujr"),
      new Wide_Wide_String'("Guru"),
      new Wide_Wide_String'("Hang"),
      new Wide_Wide_String'("Hani"),
      new Wide_Wide_String'("Hano"),
      new Wide_Wide_String'("Hatr"),
      new Wide_Wide_String'("Hebr"),
      new Wide_Wide_String'("Hira"),
      new Wide_Wide_String'("Hluw"),
      new Wide_Wide_String'("Hmng"),
      new Wide_Wide_String'("Hmnp"),
      new Wide_Wide_String'("Hrkt"),
      new Wide_Wide_String'("Hung"),
      new Wide_Wide_String'("Ital"),
      new Wide_Wide_String'("Java"),
      new Wide_Wide_String'("Kali"),
      new Wide_Wide_String'("Kana"),
      new Wide_Wide_String'("Kawi"),
      new Wide_Wide_String'("Khar"),
      new Wide_Wide_String'("Khmr"),
      new Wide_Wide_String'("Khoj"),
      new Wide_Wide_String'("Kits"),
      new Wide_Wide_String'("Knda"),
      new Wide_Wide_String'("Kthi"),
      new Wide_Wide_String'("Lana"),
      new Wide_Wide_String'("Laoo"),
      new Wide_Wide_String'("Latn"),
      new Wide_Wide_String'("Lepc"),
      new Wide_Wide_String'("Limb"),
      new Wide_Wide_String'("Lina"),
      new Wide_Wide_String'("Linb"),
      new Wide_Wide_String'("Lisu"),
      new Wide_Wide_String'("Lyci"),
      new Wide_Wide_String'("Lydi"),
      new Wide_Wide_String'("Mahj"),
      new Wide_Wide_String'("Maka"),
      new Wide_Wide_String'("Mand"),
      new Wide_Wide_String'("Mani"),
      new Wide_Wide_String'("Marc"),
      new Wide_Wide_String'("Medf"),
      new Wide_Wide_String'("Mend"),
      new Wide_Wide_String'("Merc"),
      new Wide_Wide_String'("Mero"),
      new Wide_Wide_String'("Mlym"),
      new Wide_Wide_String'("Modi"),
      new Wide_Wide_String'("Mong"),
      new Wide_Wide_String'("Mroo"),
      new Wide_Wide_String'("Mtei"),
      new Wide_Wide_String'("Mult"),
      new Wide_Wide_String'("Mymr"),
      new Wide_Wide_String'("Nagm"),
      new Wide_Wide_String'("Nand"),
      new Wide_Wide_String'("Narb"),
      new Wide_Wide_String'("Nbat"),
      new Wide_Wide_String'("Newa"),
      new Wide_Wide_String'("Nkoo"),
      new Wide_Wide_String'("Nshu"),
      new Wide_Wide_String'("Ogam"),
      new Wide_Wide_String'("Olck"),
      new Wide_Wide_String'("Orkh"),
      new Wide_Wide_String'("Orya"),
      new Wide_Wide_String'("Osge"),
      new Wide_Wide_String'("Osma"),
      new Wide_Wide_String'("Ougr"),
      new Wide_Wide_String'("Palm"),
      new Wide_Wide_String'("Pauc"),
      new Wide_Wide_String'("Perm"),
      new Wide_Wide_String'("Phag"),
      new Wide_Wide_String'("Phli"),
      new Wide_Wide_String'("Phlp"),
      new Wide_Wide_String'("Phnx"),
      new Wide_Wide_String'("Plrd"),
      new Wide_Wide_String'("Prti"),
      new Wide_Wide_String'("Rjng"),
      new Wide_Wide_String'("Rohg"),
      new Wide_Wide_String'("Runr"),
      new Wide_Wide_String'("Samr"),
      new Wide_Wide_String'("Sarb"),
      new Wide_Wide_String'("Saur"),
      new Wide_Wide_String'("Sgnw"),
      new Wide_Wide_String'("Shaw"),
      new Wide_Wide_String'("Shrd"),
      new Wide_Wide_String'("Sidd"),
      new Wide_Wide_String'("Sind"),
      new Wide_Wide_String'("Sinh"),
      new Wide_Wide_String'("Sogd"),
      new Wide_Wide_String'("Sogo"),
      new Wide_Wide_String'("Sora"),
      new Wide_Wide_String'("Soyo"),
      new Wide_Wide_String'("Sund"),
      new Wide_Wide_String'("Sylo"),
      new Wide_Wide_String'("Syrc"),
      new Wide_Wide_String'("Tagb"),
      new Wide_Wide_String'("Takr"),
      new Wide_Wide_String'("Tale"),
      new Wide_Wide_String'("Talu"),
      new Wide_Wide_String'("Taml"),
      new Wide_Wide_String'("Tang"),
      new Wide_Wide_String'("Tavt"),
      new Wide_Wide_String'("Telu"),
      new Wide_Wide_String'("Tfng"),
      new Wide_Wide_String'("Tglg"),
      new Wide_Wide_String'("Thaa"),
      new Wide_Wide_String'("Thai"),
      new Wide_Wide_String'("Tibt"),
      new Wide_Wide_String'("Tirh"),
      new Wide_Wide_String'("Tnsa"),
      new Wide_Wide_String'("Toto"),
      new Wide_Wide_String'("Ugar"),
      new Wide_Wide_String'("Vaii"),
      new Wide_Wide_String'("Vith"),
      new Wide_Wide_String'("Wara"),
      new Wide_Wide_String'("Wcho"),
      new Wide_Wide_String'("Xpeo"),
      new Wide_Wide_String'("Xsux"),
      new Wide_Wide_String'("Yezi"),
      new Wide_Wide_String'("Yiii"),
      new Wide_Wide_String'("Zanb"),
      new Wide_Wide_String'("Zinh"),
      new Wide_Wide_String'("Zyyy"),
      new Wide_Wide_String'("Zzzz"));

   type Line_Break_Alias is not null access constant Wide_Wide_String;
   Line_Break_Aliases : constant array (Line_Break) of Line_Break_Alias :=
     (new Wide_Wide_String'("AI"),
      new Wide_Wide_String'("AL"),
      new Wide_Wide_String'("B2"),
      new Wide_Wide_String'("BA"),
      new Wide_Wide_String'("BB"),
      new Wide_Wide_String'("BK"),
      new Wide_Wide_String'("CB"),
      new Wide_Wide_String'("CJ"),
      new Wide_Wide_String'("CL"),
      new Wide_Wide_String'("CM"),
      new Wide_Wide_String'("CP"),
      new Wide_Wide_String'("CR"),
      new Wide_Wide_String'("EB"),
      new Wide_Wide_String'("EM"),
      new Wide_Wide_String'("EX"),
      new Wide_Wide_String'("GL"),
      new Wide_Wide_String'("H2"),
      new Wide_Wide_String'("H3"),
      new Wide_Wide_String'("HL"),
      new Wide_Wide_String'("HY"),
      new Wide_Wide_String'("ID"),
      new Wide_Wide_String'("IN"),
      new Wide_Wide_String'("IS"),
      new Wide_Wide_String'("JL"),
      new Wide_Wide_String'("JT"),
      new Wide_Wide_String'("JV"),
      new Wide_Wide_String'("LF"),
      new Wide_Wide_String'("NL"),
      new Wide_Wide_String'("NS"),
      new Wide_Wide_String'("NU"),
      new Wide_Wide_String'("OP"),
      new Wide_Wide_String'("PO"),
      new Wide_Wide_String'("PR"),
      new Wide_Wide_String'("QU"),
      new Wide_Wide_String'("RI"),
      new Wide_Wide_String'("SA"),
      new Wide_Wide_String'("SG"),
      new Wide_Wide_String'("SP"),
      new Wide_Wide_String'("SY"),
      new Wide_Wide_String'("WJ"),
      new Wide_Wide_String'("XX"),
      new Wide_Wide_String'("ZW"),
      new Wide_Wide_String'("ZWJ"),
        -- Proposed
      new Wide_Wide_String'("AK"),
      new Wide_Wide_String'("AP"),
      new Wide_Wide_String'("AS"),
      new Wide_Wide_String'("VF"),
      new Wide_Wide_String'("VI"));
   
   type East_Asian_Width is (Ambiguous,
                             Fullwidth,
                             Halfwidth,
                             Neutral,
                             Narrow,
                             Wide);
   
   type Canonical_Combining_Class is range 0 .. 254;
   function Not_Reordered return Canonical_Combining_Class is (0);

   type Decomposition_Type is
      (Canonical,
        Compat   ,
        Circle   ,
        Final    ,
        Font      ,
        Fraction ,
        Initial   ,
        Isolated ,
        Medial   ,
        Narrow   ,
        Nobreak ,
        None      ,
        Small    ,
        Square   ,
        Sub      ,
        Super    ,
        Vertical  ,
        Wide      
        );
   
end Unicode.Properties;
