function isPrime(number) {
    let start = 2;
    const limit = Math.sqrt(number);
    while (start <= limit) {
        if (number % start++ < 1) return false;
    }
    return number > 1;
}

function primes(n) {
		const result = [];
		let i = 0;
		while (result.length < n) {
				if (isPrime(i)) {
						result.push(i);
				}
				i++;
		}
		return result;
}

console.log(primes(500000));