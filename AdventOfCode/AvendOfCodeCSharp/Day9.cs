using System;
using System.Collections.Generic;
using System.Linq;

namespace AvendOfCodeCSharp
{
    class Day9
    {
        public void SolvePart1()
        {
            const int maxturns = 71010 * 100;
            const int player = 468;
            var marbles = new LinkedList<long>();
            var current = marbles.AddFirst(0);
            var scores = new long[player];

            for (var turn = 1; turn < maxturns; turn++)
            {
                if (turn % 23 != 0)
                {
                    current = marbles.AddAfter(current.Next ?? marbles.First, turn);
                }
                else
                {
                    scores[turn % player] += turn;

                    for (var j = 0; j < 7; j++)
                        current = current.Previous ?? marbles.Last;

                    scores[turn % player] += current.Value;
                    var remove = current;
                    current = remove.Next;
                    marbles.Remove(remove);
                }
            }

            Console.WriteLine(scores.Max());
        }
    }
}
