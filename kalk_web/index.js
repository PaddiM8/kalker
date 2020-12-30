main();

async function main() {
    const kalk = await import("kalk-rs");

    try {
        console.log(kalk.evaluate("5^3").toScientificNotation().toString());
    } catch(err) {
        console.log(err);
    }
}