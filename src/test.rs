fn main() {
    let spoof = format("{}{}", "0".repeat(7), (1));
    type א = i32;
    let mut a: Option<א> = 1.into();
    //متغیر [`a`] خالی نیست.
    println!("{}", a.unwrap());
    for hebrew_letter in 'ת'..'א' {
        unreachable!("Empty range!")
    }
    /* تعليق…
طويل…      
 جدا.      */
    let long = "سلسلة
طويلة
جدا"; // 15 code points long.
    let longer = "سلسلة
طويلة
جدا
an
even
longer
string"; // 37 code points long.

    // TODO A line commentCARRIAGE RETURNspanning two paragraphs.
    // TODO A line commentLINE SEPARATOR spanning two lines of a single paragraph.
    println!("‫YouTube تابعة لشركة Google‬" /*123*/);
    println!("⁨‫YouTube تابعة لشركة Google⁩");
    a = /*א*/ (1 - 2).into();
}
