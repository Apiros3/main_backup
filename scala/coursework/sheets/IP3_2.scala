
object IP1 {

    //Question 1
    class FilterIterator[T] (test: T => Boolean, it: Iterator[T]) extends Iterator[T] {
        def hasNext : Boolean = ???
        def next() : T = ???
        var itt = it;
        var st : Iterator[T] = Iterator()
        while(itt.hasNext) {
            var tmp = itt.next()
            if (test(tmp)) {
                st = st ++ Iterator(tmp);
            }
        }
    }

    //Question 2
    trait Command[T] {
        def execute(target: T) : Option[Change]
    }
    trait Change {
        def undo()
    }
    //a)
    trait Account {
        //require balance >= 0 as DTI 
        //returns balance
        def balance : Int
        //Postcondition: balance = balance0 + num
        def deposit(num : Int) 
        //Precondition: num <= balance 
        //Postcondition: balance = balance0 - num
        def withdraw(num : Int)
    }
    //b)
    class Undo_Deposit(target: Account, amount: Int) extends Change {
        def undo() = {
            target.withdraw(amount);
        }
    }
    class DepositCommand (amount: Int) extends Command[Account] {
        require(amount > 0)
        def execute(target: Account) : Option[Change] = {
            var undo = new Undo_Deposit(target,amount)
            target.deposit(amount)
            return Some(undo)
        }
    }
    class Undo_Withdraw(target: Account, amount: Int) extends Change {
        def undo() = {
            target.deposit(amount);
        }
    }
    class WithdrawCommand (amount: Int) extends Command[Account] {
        require(amount > 0)
        def execute(target: Account) : Option[Change] = {
            if (target.balance < amount) return None 
            var undo = new Undo_Withdraw(target,amount)
            target.withdraw(amount)
            return Some(undo)
        }
    }
    //c)
    class BasicAccount(init: Int) extends Account {
        var bal = init; 
        def balance = bal;
        def deposit(num : Int) = {
            bal += num;
        }
        def withdraw(num : Int) = {
            require(num <= bal) 
            bal -= num;
        }
    }

    //Question 3 
    trait PriorityQueue {
        def isEmpty: Boolean
        def insert(e: Int)
        def remove(e: Int)
        def delMin() : Int 
    }
    class Undo_Insert(target : PriorityQueue, x : Int) extends Change {   
        def undo() = {
            target.remove(x);
        }
    }
    class InsertCommand (x : Int) extends Command[PriorityQueue] {
        def execute(que : PriorityQueue) : Option[Change] = {
            var undo = new Undo_Insert(que,x)
            que.insert(x);
            return Some(undo)
        }
    }
    class Undo_delMin(target : PriorityQueue, x : Int) extends Change {
        def undo() = {
            target.insert(x);
        }
    }
    class DelMinCommand extends Command[PriorityQueue] {
        def execute(que: PriorityQueue) : Option[Change] = {
            if (!que.isEmpty) return None 
            var tmp = que.delMin()
            var undo = new Undo_delMin(que, tmp)
            return Some(undo)
        }
    }

    //Question 4
    //a)
    class Undo_AndThen (undo_first : Option[Change], undo_second : Option[Change]) extends Change {
        def undo() = {
            undo_second.get.undo()
            undo_first.get.undo()
        }
    }
    class AndThenCommand[T] (first : Command[T], second : Command[T]) extends Command[T] {
        def execute(obj : T) : Option[Change] = {
            var f = first.execute(obj);
            if (f.isEmpty) return None 
            var s = second.execute(obj);
            if (s.isEmpty) {
                f.get.undo()
                return None 
            }
            var undo = new Undo_AndThen(f,s)
            return Some(undo)
        }
    }
    //b)
    def makeTransaction[T] (x : List[Command[T]]) : Transaction[T] = {
        return new Transaction(x)
    }
    class Undo_Transaction[T] (lis : List[Option[Change]]) extends Change {
        def undo() = {
            var ls = lis;
            while(!ls.isEmpty) {
                ls.head.get.undo()
                ls = ls.tail 
            }
        }
    }
    class Transaction[T] (x : List[Command[T]]) extends Command[T] {
        def execute(obj : T) : Option[Change] = {
            var ret : List[Option[Change]] = List()
            var i = 0;
            for (i <- 0 until x.size) {
                var tmp = x(i).execute(obj);
                if (tmp.isEmpty) {
                    //undo all actions
                    var m = new Undo_Transaction(ret)
                    m.undo()
                    return None 
                }
                ret = tmp :: ret
            }
            // println(ret.size)
            var m = new Undo_Transaction(ret)
            return Some(m)

        } 
    }

    //Question 5
    //a)
    class Undo_While (cnt: Int, cmd: Option[Change]) extends Change {
        def undo() = {
            for(i <- 0 until cnt) {
                cmd.get.undo()
            }
        }
    }
    class WhileCommand[T] (test : T => Boolean, cmd : Command[T]) extends Command[T] {
        def execute(target: T) : Option[Change] = {
            var cnt = 0;
            var udo = cmd.execute(target);
            if (udo.isEmpty) return None 
            udo.get.undo()
            while(test(target)) {
                var tmp = cmd.execute(target);
                if (tmp.isEmpty) {
                    for(i <- 0 until cnt) udo.get.undo()
                    return None 
                }
                cnt += 1;
            }
            var ret = new Undo_While(cnt,udo)
            return Some(ret)
        }
    }
    //b)
    def thold(limit : Int, que : PriorityQueue) : Boolean = {
        if (que.isEmpty) return false 
        var tmp = que.delMin()
        que.insert(tmp) //get smallest value
        if (tmp < limit) {
            return true 
        }
        else {
            return false 
        }
    }
    def threshold(limit: Int) : PriorityQueue => Boolean = {
        return (x => thold(limit,x))
    }

    //Question 6
    //No idea, ask during tutorial

    //Question 7
    //Scalacheck isn't working, but the error probably spans from decimal precision error (sqrt is a float)
    //we could check by setting a tol and seeing if the difference between the result of sqrt(n*n) and n is less than the tol

    //Question 8
    //Similar problem to above

    def main(args: Array[String]) : Unit  = {
        
        val str = 
            "Running..."
        println(str)

        //Extends Q2c
        val ac1 = new BasicAccount(50)
        val d10 = new DepositCommand(10)
        val w5 = new WithdrawCommand(5)
        // d10.execute(ac1)
        // println(ac1.balance)
        // w5.execute(ac1)
        // println(ac1.balance)
        val t = makeTransaction(List(d10,d10,w5,d10,w5))
        val c1 = t.execute(ac1)
        println(ac1.balance)
        c1.get.undo()
        println(ac1.balance)
        
    }
}