fun <T> everyNth(lst: List<T>, n: Int): List<T> {
	var index = n-1
	val tmpList = ArrayList<T>()
	while (index < lst.size) {
		tmpList.add(lst.get(index))
		index += n
	}
	val fnlList: List<T> = tmpList
	return fnlList
}

fun main(args: Array<String>) {
	val testList1 = listOf(1, 2, 3, 4, 5, 6, 7)
	val testList2 = listOf("a", "b", "c", "d", "e", "f", "g", "h", "i")
	val output1 = everyNth(testList1, 2)
	val output2 = everyNth(testList2, 2)
	for (element in output1) {
		println(element)
	}
	for (element in output2) {
		println(element)
	}
}