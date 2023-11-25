# OOP inheritance for Rust

The `oop_inheritance` crate provides a flexible Node class that can be subclassed. This crate is implemented given that the Rust language does not include support for struct inheritance.

## Example

```rust
use oop_inheritance::{Node, class};

class! {
    struct C0: Node {
        // fn x() -> f64
        // fn set_x(x: f64) -> Self
        x: f64,
        // fn x() -> Arc<f64>
        // fn set_x(x: Arc<f64>) -> Self
        ref y: f64,
    }
    // C0::new()
    fn constructor() {
        super();
    }
}

class! {
    struct C1: C0 < Node {}
    fn constructor() {
        super();
    }
}

class! {
    struct C2: C1 < C0 < Node {}
    fn constructor() {
        super();
    }
}
```