#!/usr/bin/perl

use warnings;
use strict;
use POSIX qw(uname);

sub constantly ($) {
    my $value = shift;
    return sub () {
        return $value;
    };
}

sub no_transformation (@) {
    return @_;
}

sub simple_transformer ($) {
    my $hashref = shift;
    my %hash = %$hashref;
    return sub ($$) {
        my ($command, $param) = @_;
        exists($hash{$command}) || die('Unknown key $command');
        return $hash{$command}, $param;
    };
}

sub clozure_command () {
    my ($sysname, $nodename, $release, $version, $machine) = POSIX::uname();
    # Checking POSIX::uname()[4] is problematic since it returns i386
    # on 64-bit Mac OS X. So as a hack base it on the OS instead....
    # Otherwise just call the ccl script.
    my $ospart = 'ccl';
    my $cpupart = '';
    if ($sysname =~ m/NT/) {
        $ospart = 'w';
        $cpupart = 'x86cl'
    }
    return $ospart . $cpupart;
}

my %impls = (
    'sbcl' => {
        'cmd' => constantly('sbcl'),
        'transform_arg' => \&no_transformation,
        'reorder_args' => \&no_transformation
    },
    'clozure' => {
        'cmd' => \&clozure_command,
        'transform_arg' => \&no_transformation,
        'reorder_args' => \&no_transformation
    },
    'ccl' => {
        'cmd' => \&clozure_command,
        'transform_arg' => \&no_transformation,
        'reorder_args' => \&no_transformation
    },
    'ecl' => {
        'cmd' => constantly('ecl'),
        'transform_arg' => simple_transformer({'--load' => '-load',
                                               '--eval' => '-eval'}),
        'reorder_args' => \&no_transformation
    },
    'clisp' => {
        'cmd' => constantly('clisp'),
        'transform_arg' => sub ($$) {
            my ($command, $param) = @_;
            if ($command eq '--load') {
                $command = '--eval';
                $param = "(load \"$param\")";
            }
            if ($command eq '--eval') {
                return '-x', $param;
            }
            die('Unknown key $command');
        },
        'reorder_args' => \&no_transformation
    },
    'allegro' => {
        'cmd' => constantly('alisp'),
        'transform_arg' => simple_transformer({'--load' => '-L',
                                               '--eval' => '-e'}),
        'reorder_args' => sub (@) {
            my @args = @_;
            if (scalar(@args) >= 2) {
                my $file = pop(@args);
                my $command = pop(@args);
                if ($command eq '-L') {
                    unshift(@args, '+s', $file);
                } else {
                    push(@args, $command, $file);
                }
            }
            unshift(@args, '+B');
            return @args;
        }
    }
);

my $len = scalar(@ARGV);

if ($len < 1) {
    die("Please specify a Lisp implementation");
}

my @in_args = @ARGV;
my $impl_name = shift(@in_args);
my $impl_cmd;
if (!exists($impls{$impl_name}->{'cmd'})) {
    die("Unknown Lisp implementation \"$impl_name\".");
}
$impl_cmd = $impls{$impl_name}->{'cmd'}();
my @out_args;
while (my $in_arg = shift(@in_args)) {
    my $param;
    my @result;
    if ($in_arg eq '--load' || $in_arg eq '--eval') {
        $param = shift(@in_args);
        @result = $impls{$impl_name}->{'transform_arg'}($in_arg, $param);
        push(@out_args, @result);
    } else {
        push(@out_args, $in_arg);
    }

    if ($in_arg eq '--') {
        last;
    }
}
@out_args = $impls{$impl_name}->{'reorder_args'}(@out_args);
push(@out_args, @in_args);

exec $impl_cmd, @out_args;
