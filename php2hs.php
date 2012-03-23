<?php
// Take the data/lang.dat file distributed with Text_LanguageDetect (which is
// assumed installed) and print a haskell program on stdout which will make the
// serialized file used by this module.

require 'Text/LanguageDetect.php';
$ld = new Text_LanguageDetect();
$x = unserialize(file_get_contents($ld->_get_data_loc('lang.dat')));

$iso = new Text_LanguageDetect_ISO639();

mb_internal_encoding("utf8");

function f($trigram, $n) {
    return str_replace("'", "\\'", mb_substr($trigram, $n, 1));
}

echo "import Data.Map\nimport Data.Serialize\nimport Data.ByteString.Char8 as B\nmain=B.writeFile \"lang.dat\"\$encode langData\nlangData :: Map String (Map (Char, Char, Char) Int)\nlangData=fromList [";
$out = "";
foreach ($x["trigram"] as $lang => $ranks) {
    if (($tmp = $iso->nameToCode2($lang)) !== null ) {
	    $lang = $tmp;
    } else {
	    $lang = $iso->nameToCode3($lang);
    }
    $out .= "(\"".$lang."\",fromList [";
    foreach ($ranks as $trigram => $rank) {
        $out .= "(('".f($trigram, 0)."','".
                      f($trigram, 1)."','".
                      f($trigram, 2)."'),".$rank."),";
    }
    $out = substr($out, 0, -1)."]),";
}
$out = substr($out, 0, -1)."]";
echo $out;
