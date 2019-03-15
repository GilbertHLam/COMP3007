//Gilbert Lam
//101044511
import java.util.*; 

public class comp3007_w19_101044511_a1_1 {

	public static void main(String []args) {
		String stringCase;
		String pigLatin;

		System.out.print("Enter Snake Case String: ");
		Scanner scanner = new Scanner(System.in);
		stringCase = scanner.nextLine();
		System.out.println("String is "+ stringCase);
		if(partA(stringCase)) {
			ArrayList<String> list = partF(partC(stringCase));
			System.out.println(list);
		}
		
		scanner.close();
   }

	public static boolean partA(String stringCase) {

		if(stringCase.equals("")) {
			return true;
		} else if (stringCase.charAt(0) >= 'A' && stringCase.charAt(0) <= 'Z' ) {
			return false;
		} else {
			return partA(stringCase.substring(1));
		}
   }

	public static int partB(String stringCase) {

		if(stringCase.equals("")) {
			return -100;
		} else if (stringCase.charAt(0) == '_' ) {
			return 0;
		} else {
			return 1 + partB(stringCase.substring(1));
		}
	}

	public static ArrayList<String> partC(String stringCase) {

		if(partB(stringCase.substring(1)) < 0) {
			return new ArrayList<String>(
				Arrays.asList(stringCase)
			);
		} else {
			ArrayList list = new ArrayList<String>();
			list.add(stringCase.substring(0, partB(stringCase)));
			list.addAll(partC(stringCase.substring(partB(stringCase)+1)));
			return list;
		} 

	}

	public static int partD(String stringCase){
		if(stringCase.equals("")) {
			return -100;
		} else if (isVowel(stringCase.charAt(0))) {
			return 0;
		} else {
			return 1 + partD(stringCase.substring(1));
		}
	}

	public static String partE(String stringCase){
		String constanants = stringCase.substring(0, partD(stringCase));
		return stringCase.substring(partD(stringCase)) + constanants + "ay";
	}

	public static ArrayList<String> partF(ArrayList<String> list){

		if(list.isEmpty()) {
			return new ArrayList<String>();
		} else {
			ArrayList returnList = new ArrayList<String>();
			returnList.add(partE(list.get(0)));
			list.remove(0);
			returnList.addAll(partF(list));
			return returnList;
		} 
	}

	public static boolean isVowel(char character) {
		if((character == 'a') || (character == 'e') || (character == 'i') || (character == 'o') || (character == 'u')) {
			return true;
		}
		return false;
	}



}