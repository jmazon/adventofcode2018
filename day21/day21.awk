NR == 1

{
    count++;
    if (seen[$0]) {
        print last;
        print "(Examined:) ", count;
        exit;
    }
    seen[$0]++;
    last = $0;
}
