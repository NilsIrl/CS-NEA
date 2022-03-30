fn main() {
    let mut biggest = 0;
    for i in 3..=100000 {
        let mut bail = false;
        for j in 3..i {
            if i % j == 0 {
                bail = true;
                break;
            }
        }
        if !bail {
            biggest = i;
        }
    }

    println!("{}", biggest);
}
