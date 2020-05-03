use strict;
use warnings;

use feature qw(say);

sub strings {

    say "hello\nthere";
    say 'general kenobi';

    say q(hello
there);
    say q(general kenobi);

    say qq(hello\nthere);
}

sub loops {

    my $hashmap = {
        a => 1,
        b => 0,
    };
    my $other_hashmap = {};

    for my $key ( sort keys %{$hashmap} ) {
        if ( $hashmap->{$key} ) {
            say $key
        }
    }
    for my $key ( sort keys %{$hashmap} ) {
        if ( $other_hashmap->{$key} ) {
            say $key
        }
    }

}


strings();
loops();

