main();

async function main() {
    const kalk = await import("../kalk/pkg");

    try {
        const context = new kalk.Context();
        console.log(context.evaluate("sum(1, 5, n)").toScientificNotation().toString());
    } catch(err) {
        console.log(err);
    }
}