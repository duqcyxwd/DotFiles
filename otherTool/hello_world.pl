#!/usr/bin/perl -w
 
$num_args = $#ARGV + 1;
print "Script for dely application start\n";
if ($num_args > 1) {
    print "\nOnly need time \n";
    exit;
} elsif ($num_args == 1) {
    $break_time=$ARGV[0];
} else {
    $break_time=10;
}

system ("pkill 'Time Out Free'");
$num = $break_time;

while($num--){
    sleep(1);
}

print "Break for $break_time\n";
print "start Timeout Application\n";

system ("open -a /Applications/'Time Out Free.app'");
# system ("open -a /Applications/'Font Book.app'");

