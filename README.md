# Poco

A tiny, dynamic, embeddable language.

```rust
fn prefix_sum(nums) {
    sums = []
    for i in 0..len(nums) {
        sums[i] = nums[i]
        if i > 0 {
            sums[i] = sums[i] + sums[i - 1]
        }
    }
    return sums
}
print(prefix_sum([1, 2, 3, 4, 5]))
```

Try it by running it:

```sh
poco hello.poco
```

Or embed it as a Rust library:

```rust
poco::eval_src("print(\"Hello world\")!")
```
