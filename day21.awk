{
    if (seen[$0]) {
        print "Found: ", last;
        exit;
    }
}
seen[$0]++; last = $0;
