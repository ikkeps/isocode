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

sub hashmaps {

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
    for my $k ( sort keys %{$hashmap} ) {
        if ( $other_hashmap->{$k} ) {
            say $k;
        }
    }
}

sub errors {
    eval {
        say "its ok";
        1;
    }
    or do {
        my $error = $@ || 'Zombie error';
        say "it was not ok";
    };
}

strings();
hashmaps();

