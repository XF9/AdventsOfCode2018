using System;
using System.Collections.Generic;
using System.Linq;

namespace AvendOfCodeCSharp
{
    class Day8
    {
        private int[] GetInput()
        {
            return System.IO.File
                .ReadAllText("Day8Input.txt")
                .Split(' ')
                .Select(Int32.Parse)
                .ToArray();
        }

        public void SolvePart1()
        {
            var node = Node.BuildFromInput(GetInput());
            Console.WriteLine(node.Score);
        }

        public void SolvePart2()
        {
            var node = Node.BuildFromInput(GetInput());
            Console.WriteLine(node.Score2());
        }

        private class Node
        {
            private IEnumerable<Node> Childen { get; set; }
            private IEnumerable<int> Meta { get; set; }
            private int Length { get; set; }

            public int Score => Meta.Sum() + Childen.Select(n => n.Score).Sum();

            public int Score2()
            {
                if (!Childen.Any())
                    return Score;

                var score = 0;
                foreach (var meta in Meta)
                {
                    var index = meta - 1;
                    if(index < 0 || Childen.Count() <= index)
                        continue;

                    score += Childen.ElementAt(index).Score2();
                }

                return score;
            }

            public static Node BuildFromInput(int[] input)
            {
                var childList = new List<Node>();
                var data = input.Skip(2).ToArray();

                for (var i = 0; i < input[0]; i++)
                {
                    var child = BuildFromInput(data.ToArray());
                    childList.Add(child);
                    data = data.Skip(child.Length).ToArray();
                }

                return new Node
                {
                    Childen = childList,
                    Meta = data.Take(input[1]),
                    Length = 2 + childList.Sum(c => c.Length) + input[1]
                };
            }
        }
    }
}
