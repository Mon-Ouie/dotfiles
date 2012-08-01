#!/usr/bin/perl -w

# Maildir flags are:
#         D (draft)
#         F (flagged)
#         R (replied)
#         S (seen)
#         T (trashed)
# and must occur in ASCII order.
#
# flagmatchre = re.compile(':.*2,([A-Z]+)')
#
# filename:2,F   => .nnmaildir/marks/tick/filename
# filename:2,R   => .nnmaildir/marks/reply/filename
# filename:2,S   => .nnmaildir/marks/read/filename

use strict;
use File::Basename;
use Getopt::Long;
$Getopt::Long::ignorecase = 0;

my $from_gnus = 0;
my $from_maildir = 0;
my $dir = "~/.Maildir";
GetOptions('-g' => \$from_gnus,
           '-m' => \$from_maildir,
           '-d=s' => \$dir);

if (! ($from_gnus ^ $from_maildir)) {
  die "Usage: sync_nnmaildir -g [-f]\n   or: sync_nnmaildir -m [-v -f]\n";
}

for (glob "$dir/*") {
  my $mb = $_;
  mkdir "$mb/.nnmaildir";
  mkdir "$mb/.nnmaildir/marks";

  for (glob "$mb/cur/*") {
    my $file = $_;

    /(.*)\/cur\/(.*?):.*2,(.*)$/;
    my $path = $1;
    my $message = $2;
    my $flags = $3;

    if ($from_maildir) {
      # Sync ticked flags
      if ($flags =~ /F/) {
        mkdir "$path/.nnmaildir/marks/tick";
        my $dst = "$path/.nnmaildir/marks/tick/$message";
        link "$file","$dst"
          and print "Added mail in $mb to nnmaildir ticks\n";
      } else {
        my $dst = "$path/.nnmaildir/marks/tick/$message";
        unlink "$dst"
          and print "Removed mail in $mb from nnmaildir ticks\n";
      }

      # Sync replied flags
      if ($flags =~ /R/) {
        mkdir "$path/.nnmaildir/marks/reply";
        my $dst = "$path/.nnmaildir/marks/reply/$message";
        link "$file","$dst"
          and print "Added mail in $mb to nnmaildir replies\n";
      } else {
        my $dst = "$path/.nnmaildir/marks/reply/$message";
        unlink "$dst"
          and print "Removed mail in $mb from nnmaildir replies\n";
      }

      # Sync read flags
      if ($flags =~ /S/) {
        mkdir "$path/.nnmaildir/marks/read";
        my $dst = "$path/.nnmaildir/marks/read/$message";
        link "$file","$dst"
          and print "Added mail in $mb to nnmaildir seen\n";
      } else {
        my $dst = "$path/.nnmaildir/marks/read/$message";
        unlink "$dst"
          and print "Removed mail in $mb from nnmaildir seen\n";
      }
    } elsif ($from_gnus) {
      my $new_flags = '';

      if (-e "$path/.nnmaildir/marks/tick/$message") {
        $new_flags = $new_flags . 'F';
      }
      if (-e "$path/.nnmaildir/marks/reply/$message") {
        $new_flags = $new_flags . 'R';
      }
      if (-e "$path/.nnmaildir/marks/read/$message") {
        $new_flags = $new_flags . 'S';
      }

      if ($new_flags ne $flags) {
        rename "$file", "$path/cur/$message:2,$new_flags"
          and print "Marked mail in $mb as $new_flags\n";
      }
    }
  }
}
