use core::ptr::NonNull;

#[repr(align(2))]
pub struct Align2(u16);

impl Align2 {
    #[allow(dead_code)]
    pub fn sentinel() -> NonNull<Self> {
        Align4::sentinel().cast()
    }
}

#[repr(align(4))]
pub struct Align4(u32);

impl Align4 {
    pub fn sentinel() -> NonNull<Self> {
        static SENTINEL: Align4 = Self(0);
        NonNull::from(&SENTINEL)
    }
}
