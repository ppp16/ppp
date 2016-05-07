class HelloWorldJava {
	static String greeting4 = "Hi, Galaxy!";
	static Double pGreeting4TodayIfSunnyToday = 0.5;
	static Double pGreeting4TomorrowIfSunnyTomorrow = 0.5;

	static Double pGreeting1TodayIfSunnyToday = 0.6;
	static Double pGreeting2TodayIfSunnyToday = 0.4;
	
	static Double pGreeting1TomorrowIfSunnyTomorrow = 0.3;
	static Double pGreeting2TomorrowIfSunnyTomorrow = 0.2;
	
	
	static String greeting1 = "Hello, world!";
	static String greeting2 = "Howdy, universe!";
	static String greeting3 = "Oh no, not again";
	static Double pSunnyToday = 0.2;
	static Double pNotSunnyToday = 0.8;
	static Double pSunnyTomorrowIfSunnyToday = 0.8;
	static Double pNotSunnyTomorrowIfSunnyToday = 0.2;
	static Double pSunnyTomorrowIfNotSunnyToday = 0.05;
	static Double pNotSunnyTomorrowIfNotSunnyToday = 0.95;
	static Double pGreeting1TodayIfNotSunnyToday = 0.2;
	static Double pGreeting3IfNotSunnyToday = 0.8;
	static Double pGreeting1TomorrowIfNotSunnyTomorrow = 0.2;
	static Double pGreeting3TomorrowIfNotSunnyTomorrow = 0.8;

	static void predict() {
		Double pGreeting4Today = pSunnyToday * pGreeting4TodayIfSunnyToday;
		System.out.println("Today's greeting is " + greeting4
				+ "with probability " + pGreeting4Today + ".");
	}

	static void infer() {
		Double pSunnyTodayAndGreeting4Today = 1.0;
		Double pSunnyTodayGivenGreeting4Today = pSunnyTodayAndGreeting4Today;
		System.out.println("If today's greeting is " + greeting4
				+ ", today's weather is sunny with probability "
				+ pSunnyTodayGivenGreeting4Today + ".");
	}

	static void learnAndPredict() {
		Double pSunnyTodayAndGreeting4Today = 1.0;
		Double pSunnyTodayGivenGreeting4Today = pSunnyTodayAndGreeting4Today;
		Double pNotSunnyTodayGivenGreeting4Today = 1 - pSunnyTodayGivenGreeting4Today;
		Double pSunnyTomorrowGivenGreeting4Today = pSunnyTodayGivenGreeting4Today
				* pSunnyTomorrowIfSunnyToday
				+ pNotSunnyTodayGivenGreeting4Today
				* pSunnyTomorrowIfNotSunnyToday;
		Double pGreeting4TomorrowGivenGreeting4Today = pSunnyTomorrowGivenGreeting4Today
				* pGreeting4TomorrowIfSunnyTomorrow;
		System.out.println("If today's greeting is " + greeting4
				+ ", tomorrow's greeting will be " + greeting4
				+ " with probability " + pGreeting4TomorrowGivenGreeting4Today);
	}

	public static void main(String[] args) {
		predict();
		infer();
		learnAndPredict();
	}
}