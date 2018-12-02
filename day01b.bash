paste <(yes last) <(while cat day01.in;do :;done)|bc|perl -ne'$g{$_}++&&die$_'
