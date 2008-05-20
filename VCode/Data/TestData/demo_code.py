class SortingStrategy:
   def sort(self, list):
      pass

class BubbleSorter(SortingStrategy):
   def sort(self, list):
      if len(list) == 0:
         return list
      for index1 in range(len(list) - 1):
         for index2 in range(index1 + 1, len(list)):
            if list[index1] < list[index2]:
               self.swap_elements(index1, index2, list)

   def swap_elements(self, index1, index2, list):
      temporary = list[index1]
      list[index1] = list[index2]
      list[index2] = temporary

some_list = [1, - 5, 2, 3, 1]
print BubbleSorter().sort(some_list)
