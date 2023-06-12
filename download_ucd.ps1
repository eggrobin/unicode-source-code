foreach ($version in @("4.0.0",
                       "4.1.0",
                       "5.0.0",
                       "5.1.0",
                       "5.2.0",
                       "6.0.0",
                       "6.1.0",
                       "6.2.0",
                       "6.3.0",
                       "7.0.0",
                       "8.0.0",
                       "9.0.0",
                       "10.0.0",
                       "11.0.0",
                       "12.0.0",
                       "12.1.0",
                       "13.0.0",
                       "14.0.0",
                       "15.0.0",
                       "15.1.0")) {
  New-Item -Path $version -ItemType Directory -Force
  New-Item -Path $version\extracted -ItemType Directory -Force
  New-Item -Path $version\emoji -ItemType Directory -Force
  foreach (
      $f in @("extracted/DerivedBidiClass",
              "extracted/DerivedGeneralCategory",
              "LineBreak",
              "extracted/DerivedEastAsianWidth",
              "UnicodeData",
              "SpecialCasing",
              "CaseFolding",
              "DerivedCoreProperties",
              "PropList",
              "extracted/DerivedBinaryProperties",
              "emoji/emoji-data",
              "DerivedNormalizationProps")) {
    Invoke-WebRequest "https://www.unicode.org/Public/$version/ucd/$f.txt" -OutFile "$version/$f.txt"
  }
}