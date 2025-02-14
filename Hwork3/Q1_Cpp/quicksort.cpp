// Kevin Gutierrez
// CSCI 330 Programming Language Concepts
// Quicksort Cpp implementation - no recursion

using namespace std;

#include <iostream>
#include <stack>
#include <vector>
#include <algorithm>
using namespace std;

int partition(int array[], int startIndex, int endIndex)
{
	// select the last element as a pivot from the array
	int pivot = array[endIndex];

	// elements smaller than the pivot element goes to the left of `pivotIndex`
	// elements greater than the pivot element goes to the right of `pivotIndex`
	// equal elements can go on either side of the pivotIndex
	int pivotIndex = startIndex;

	// if an element is less than or equal to the pivot, we will increase the 'pivotIndex' and we will place that element before the pivot.
	for (int i = startIndex; i < endIndex; i++)
	{
		if (array[i] <= pivot)
		{
			swap(array[i], array[pivotIndex]);
			pivotIndex++;
		}
	}

	// swap `pivotIndex` with pivot element
	swap(array[pivotIndex], array[endIndex]);

	return pivotIndex;
}

void iterative_quicksort(int array[], int sizeofArray)
{
	// stack for storing start and end Index of the subarrays
	stack<pair<int, int>> startAndEndIndexOfSubarrays;

	// starting index of the given array
	int startIndex = 0;
	// ending index of the given array
	int endIndex = sizeofArray - 1;

	// pushing the start and end index of the array into the stack
	startAndEndIndexOfSubarrays.push({startIndex, endIndex});

	while (!startAndEndIndexOfSubarrays.empty())
	{
		// removing the top pair from the array and get the starting
		// and ending index of the subarray
		startIndex = startAndEndIndexOfSubarrays.top().first;
		endIndex = startAndEndIndexOfSubarrays.top().second;
		startAndEndIndexOfSubarrays.pop();

		// partitioning the elements around pivot
		int pivotIdx = partition(array, startIndex, endIndex);

		// push subarray indices to stack which has elements smaller than the current pivot
		if (pivotIdx - 1 > startIndex)
		{
			startAndEndIndexOfSubarrays.push({startIndex, pivotIdx - 1});
		}

		// push subarray indices to stack which has elements greater than the current pivot
		if (pivotIdx + 1 < endIndex)
		{
			startAndEndIndexOfSubarrays.push({pivotIdx + 1, endIndex});
		}
	}
}

int main()
{

	int sizeofArray; // size of the array
	cout << "Enter the number of elements to sort: ";
	cin >> sizeofArray;

	int array[sizeofArray]; // arr is the array to be sorted

	cout << "Enter the elements of the array to be sorted:" << endl;

	for (int i = 0; i < sizeofArray; i++)
	{
		cin >> array[i];
	}

	iterative_quicksort(array, sizeofArray);

	// print the sorted array

	cout << "Sorted array:" << endl;
	for (int i = 0; i < sizeofArray; i++)
	{
		cout << array[i] << " ";
	}

	return 0;
}