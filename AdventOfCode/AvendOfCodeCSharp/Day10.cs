using System;
using System.Collections.Generic;
using System.Linq;

namespace AvendOfCodeCSharp
{
    class Day10
    {
        public void SolvePart1()
        {
            var data = GetInput().ToList();

            var exit = false;
            var iteration = 0;
            do
            {
                var key = Console.ReadKey().Key;
                switch (key)
                {
                    case ConsoleKey.Spacebar:
                        do
                        {
                            foreach (var point in data)
                                point.MoveForward();
                            iteration++;
                        } while (data.Max(p => p.Y) - data.Min(p => p.Y) > 20);

                        break;
                    default:
                        exit = true;
                        break;
                }
                PrintInput(data, iteration);
            } while (!exit);
        }

        private IEnumerable<Point> GetInput()
        {
            return System.IO.File.ReadAllLines("Day10Input.txt")
                .Select(line => line.Split(new string[] {"position=<", ",", "> velocity=<", ">"}, StringSplitOptions.RemoveEmptyEntries))
                .Select(lineData => new Point
                {
                    X = int.Parse(lineData[0].Trim()),
                    Y = int.Parse(lineData[1].Trim()),
                    VelocityX = int.Parse(lineData[2].Trim()),
                    VelocityY = int.Parse(lineData[3].Trim())
                });
        }

        private void PrintInput(List<Point> points, int itteration)
        {
            Console.WriteLine();
            Console.WriteLine($"iteration {itteration}");
            for(var y = points.Min(p => p.Y); y <= points.Max(p => p.Y); y++)
            {
                for (var x = points.Min(p => p.X); x <= points.Max(p => p.X); x++)
                    Console.Write(points.Any(p => p.X == x && p.Y == y) ? "#" : " ");
                Console.Write("\n");
            }
        }

        private class Point
        {
            public int X { get; set; }
            public int Y { get; set; }

            public int VelocityX { private get; set; }
            public int VelocityY { private get; set; }

            public void MoveForward()
            {
                X += VelocityX;
                Y += VelocityY;
            }
        }
    }
}
